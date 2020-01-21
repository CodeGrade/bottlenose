class Course < ApplicationRecord
  belongs_to :term

  has_many :sections, dependent: :destroy, inverse_of: :course
  has_many :registrations, dependent: :destroy
  has_many :users, through: :registrations

  has_many :reg_requests, dependent: :destroy

  has_many :assignments, dependent: :restrict_with_error
  has_many :submissions, through: :assignments
  has_many :teams,       dependent: :destroy
  has_many :teamsets, dependent: :destroy

  belongs_to :lateness_config

  validates :term_id, presence: true
  validates :lateness_config, presence: true
  validates :name, length: { minimum: 2 }, uniqueness: { scope: :term_id }
  validate  :has_sections

  validates_associated :sections

  validate :unique_section_crns

  def unique_section_crns
    counts = self.sections.group_by{|s| s.crn}.map{|k, v| [k, v.size]}.select{|k, v| v > 1}
    if counts.empty?
      true
    else
      counts.each do |crn, count|
        self.errors.add(:base, "CRN #{crn} is used #{count} times")
      end
      false
    end
  end
    

  accepts_nested_attributes_for :sections, allow_destroy: true
  accepts_nested_attributes_for :lateness_config

  after_create :register_profs

  def has_sections
    if sections.empty?
      errors.add(:sections, "can't be empty")
    end
  end

  def section_types
    self.sections.map(&:type).uniq.sort_by{|t| Section::types[t]}.map{|t| [t, Section::types[t]]}
  end

  def sections_by_type
    self.sections.group_by(&:type)
  end

  def register_profs
    # Can't do this before create, because self.id isn't set yet
    # Can't do this using just Registration.new and RegistrationSection.new, because
    # they aren't owned by anything owned by self, so they don't get saved
    if registrations.empty?
      sections.each do |sec|
        begin
          r = Registration.find_or_create_by(user_id: sec.instructor_id, course_id: self.id)
          r.role = "professor"
          r.show_in_lists = false
          r.new_sections = [sec.id]
          r.save!
        rescue Exception => e
          errors[:base] << "Could not register #{sec.instructor.name} for course: #{e}"
        end
        begin
          rs = RegistrationSection.create!(
            registration: r,
            section: sec
          )
        rescue Exception => e
          errors[:base] << "Could not register #{sec.instructor.name} for section #{sec.id}: #{e}"
        end
      end
    end
    raise ActiveRecord::Rollback if errors[:base].count > 0
  end

  def registered_by?(user, as: nil)
    return false if user.nil?
    registration = Registration.find_by(course_id: self.id, user_id: user.id)
    return false if registration.nil?
    if as
      as == registration.role
    else
      registration.role == 'assistant' || registration.role == 'professor'
    end
  end

  def active_registrations
    registrations.where(show_in_lists: true).joins(:user).order("users.last_name", "users.first_name")
  end

  def active_teams
    self.teams.where(Team.active_query, Date.current, Date.current)
  end

  def students
    users.where("registrations.role": Registration::roles["student"])
  end

  def users_with_registrations(for_users = users)
    for_users.joins("JOIN registration_sections ON registrations.id = registration_sections.registration_id")
      .joins("JOIN sections ON registration_sections.section_id = sections.id")
  end
  
  def students_with_registrations
    users_with_registrations(students)
  end

  def users_with_drop_info(for_users = nil)
    look_for = users
    if for_users.is_a? Array
      look_for = look_for.where(id: for_users.map(&:id))
    elsif for_users.is_a? User
      look_for = look_for.where(id: for_users.id)
    elsif for_users
      look_for = look_for.where(id: for_users.pluck(:id))
    end
    look_for.select("users.*", "registrations.dropped_date as dropped_date", "registrations.id as reg_id")
  end

  def students_with_drop_info
    users_with_drop_info(students)
  end

  def professors
    users.where("registrations.role": Registration::roles["professor"])
  end

  def graders
    users.where("registrations.role": Registration::roles["grader"])
  end

  def staff
    users
      .where("registrations.role <> #{Registration::roles["student"]}")
      .where("registrations.dropped_date is null")
  end

  def sorted_staff
    staff.select("users.*", "registrations.role as role")
      .sort_by{|s| [0 - s.role, s.sort_name]}
  end

  def first_professor
    professors.first
  end

  def assignments_sorted
    self.assignments.to_a.sort_by(&:due_date)
  end

  def all_partners
    team_users = TeamUser.where(team: teams).group_by(&:team_id)
    users_teams = teams.map do |t|
      team_users[t.id].map do |tu|
        [tu.user_id, t.id]
      end
    end.flatten(1).group_by(&:first).map{|k,v| [k,v.map(&:last)]}.to_h
    users_teams.map do |uid, tids|
      [uid, tids.map do |tid|
         team_users[tid].map{|tu| [tu.user_id, tid]}
       end.flatten(1).group_by(&:first).map{|k,v| [k,v.map(&:last)]}.to_h]
    end.to_h
  end

  def score_summary(for_students = nil)
    if for_students.nil?
      for_students = self.students
    end
    assns = self.assignments.where("available < ?", DateTime.current)
    effective_due_dates =
      multi_group_by(
        assns.map{|a| a.effective_due_dates(for_students, true).map{|uid, due| [uid, a.id, a, due]}}.flatten(1),
        [:first, :second], true)
    subs_by_user = UsedSub.where(user: for_students, assignment: assns)
      .joins(:submission)
      .select(:user_id, :assignment_id, :score, :submission_id)
      .group_by(&:user_id)
    assns = assns.map{|a| [a.id, a]}.to_h
    extras, regulars = assns.values.partition(&:extra_credit)
    total_points = self.assignments.reject(&:extra_credit).sum(&:points_available)
    # assume a default of 100 points, but there might be extra credit assignments that push the total above 100
    total_points = [100.0, total_points].max
    ans = self.users_with_drop_info(for_students).sort_by(&:sort_name).map do |s|
      dropped = s.dropped_date
      used = (subs_by_user[s.id] || []).map{|u| [u.assignment_id, u]}.to_h
      # This is customized per student, because each student might have their own extensions
      due_in_future = (effective_due_dates[s.id] || {})
               .select{|_, (_, _, a, due)| due > DateTime.current}
               .map{|_, (_, _, a, _)| [a.id, a]}.to_h
      reg = {done: {earned: 0.0, avail: 0.0}, pending: {avail: 0.0, names: []}, unsub: {avail: 0.0, names: []}}
      extra = {done: {earned: 0.0, avail: 0.0}, pending: {avail: 0.0, names: []}, unsub: {avail: 0.0, names: []}}
      assns.each do |aid, a|
        if a.points_available != 0
          sub = used[aid]
          if !a.extra_credit
            if sub&.score
              reg[:done][:earned] += sub.score * a.points_available / 100.0
              reg[:done][:avail] += a.points_available
            elsif sub
              reg[:pending][:avail] += a.points_available
              reg[:pending][:names].push a.name
            elsif due_in_future[aid]
              reg[:unsub][:avail] += a.points_available
              reg[:unsub][:names].push a.name
            else
              reg[:done][:avail] += a.points_available
            end
          else
            if sub&.score
              extra[:done][:earned] += sub.score * a.points_available / 100.0
              extra[:done][:avail] += a.points_available
            elsif sub
              extra[:pending][:avail] += a.points_available
              extra[:pending][:names].push a.name
            elsif due_in_future[aid]
              extra[:unsub][:avail] += a.points_available
              extra[:unsub][:names].push a.name
            else
              extra[:done][:avail] += a.points_available
            end
          end
        end
      end
      min = reg[:done][:earned] * (100.0 / total_points) + extra[:done][:earned]
      cur = (100.0 * min / (reg[:done][:avail] + reg[:unsub][:avail])) * (100.0 / total_points)
      remaining = total_points - reg[:done][:avail] - reg[:pending][:avail] - reg[:unsub][:avail]
      max = min + remaining + reg[:pending][:avail] + reg[:unsub][:avail] + extra[:pending][:avail] + extra[:unsub][:avail]
      {s: s, dropped: dropped, min: min, cur: cur, max: max,
       pending: reg[:pending][:avail] + extra[:pending][:avail],
       pending_names: reg[:pending][:names] + extra[:pending][:names],
       unsub: reg[:unsub][:avail] + extra[:unsub][:avail],
       unsub_names: reg[:unsub][:names] + extra[:unsub][:names],
       remaining: remaining, used: used}
    end
    ans
  end

  def grading_assigned_for(user)
    GraderAllocation
      .where(who_grades_id: user.id)
      .where(course: self)
      .where(grading_completed: nil)
      .group_by(&:assignment_id)
  end

  def grading_done_for(user)
    GraderAllocation
      .where(who_grades_id: user.id)
      .where(course: self)
      .where.not(grading_completed: nil)
      .joins(:submission)
      .where("submissions.score": nil)
      .group_by(&:assignment_id)
  end

  def pending_grading
    multi_group_by(pending_grading_query, [:assignment_id, :submission_id, :id])
  end
  def pending_grading_query
    # only use submissions that are being used for grading
    # only pick submissions from this course
    # only pick non-staff submissions
    # hang on to the assignment id
    # only keep unfinished graders
    # sort the assignments
    # group by assignment and submission id
    Grade
      .joins("INNER JOIN used_subs ON grades.submission_id = used_subs.submission_id")
      .joins("INNER JOIN assignments ON used_subs.assignment_id = assignments.id")
      .joins("INNER JOIN registrations ON used_subs.user_id = registrations.user_id")
      .where("assignments.course_id": self.id)
      .select("grades.*", "used_subs.assignment_id", "assignments.due_date")
      .joins("INNER JOIN users ON used_subs.user_id = users.id")
      .distinct.select("users.name AS user_name")
      .where(score: nil)
      .where("registrations.role": Registration::roles["student"])
      .order("assignments.due_date", "users.name")
  end

  def missing_grading
    expected = assignments.joins(:graders).select(:id, "graders.id as grader_id").group_by(&:id)
               .map{|a_id, graders| [a_id, graders.map(&:grader_id)]}.to_h
    used_subs = Submission.where(id: UsedSub.where(assignment_id: assignments.map(&:id)).select(:submission_id))
    sub_ids_missing = used_subs.joins(:grades)
                      .select("submissions.id", "submissions.assignment_id", "grades.grader_id as grader_id")
                      .group_by(&:id).select do |s_id, info|
      !info.map(&:grader_id).to_set.superset?(expected[info.first.assignment_id].to_set)
    end.keys
    used_subs.where(id: sub_ids_missing)
  end

  def abnormal_subs
    abnormals = {}
    people = self.users_with_drop_info.where("registrations.dropped_date IS ?", nil).to_a
    assns = self.assignments_sorted
    all_subs = Assignment.submissions_for(people, assns).group_by(&:assignment_id)
    used_subs = UsedSub.where(assignment_id: assns.map(&:id)).group_by(&:assignment_id)
    assns.each do |a|
      next unless all_subs[a.id]
      a_subs = all_subs[a.id].group_by(&:for_user)
      used = used_subs[a.id]&.map{|u| [u.user_id, u]}&.to_h
      people.each do |p|
        subs = a_subs[p.id]
        if (subs&.count.to_i > 0) && (used.nil? || used[p.id].nil?)
          abnormals[a] = [] unless abnormals[a]
          abnormals[a].push p
        end
      end
    end

    abnormals
  end

  def unpublished_grades
    multi_group_by(unpublished_grades_query, [:assignment_id])
  end
  def unpublished_grades_query
    Submission
      .where.not("submissions.id": pending_grading_query.map(&:submission_id) + missing_grading.map(&:id))
      .joins("INNER JOIN used_subs ON submissions.id = used_subs.submission_id")
      .where("used_subs.assignment_id": assignments_sorted.map(&:id))
      .joins("INNER JOIN grades ON grades.submission_id = submissions.id")
      .where.not("grades.score": nil)
      .where("grades.available": false)
      .joins("INNER JOIN users ON used_subs.user_id = users.id")
      .order("users.name")
      .select("DISTINCT submissions.*", "users.name AS user_name")
  end
end
