class Course < ApplicationRecord
  belongs_to :term

  has_many :course_sections, dependent: :destroy
  has_many :registrations, dependent: :destroy
  has_many :users, through: :registrations

  has_many :reg_requests, dependent: :destroy

  has_many :assignments, dependent: :restrict_with_error
  has_many :submissions, through: :assignments
  has_many :teams,       dependent: :destroy

  belongs_to :lateness_config
  validates :lateness_config, presence: true
  validate :valid_lateness_config

  validates :name,    :length      => { :minimum => 2 },
                      :uniqueness  => true

  validates :term_id, presence: true

  def valid_lateness_config
    if !self.lateness_config.nil? && !self.lateness_config.valid?
      self.lateness_config.errors.full_messages.each do |m|
        @errors[:base] << m
      end
    end
  end

  def registered_by?(user, as: nil)
    return false if user.nil?
    registration = Registration.find_by_course_id_and_user_id(self.id, user.id)
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

  def students
    users.where("registrations.role": RegRequest::roles["student"])
  end

  def users_with_drop_info(for_users = nil)
    look_for = users
    if for_users
      look_for = look_for.where(id: for_users.map(&:id))
    end
    look_for.select("users.*", "registrations.dropped_date")
  end

  def students_with_drop_info(for_users = nil)
    look_for = students
    if for_users
      look_for = look_for.where(id: for_users.map(&:id))
    end
    look_for.select("users.*", "registrations.dropped_date")
  end

  def professors
    users.where("registrations.role": RegRequest::roles["professor"])
  end

  def graders
    users.where("registrations.role": RegRequest::roles["grader"])
  end

  def staff
    users
      .where("registrations.role <> #{RegRequest::roles["student"]}")
      .where("registrations.dropped_date is null")
  end

  def first_professor
    professors.first
  end

  def add_registration(username, crn, role = :student)

    # TODO: Check LDAP for user.
    uu = User.find_by(username: username)
    if uu.nil?
      res = Devise::LDAP::Adapter.get_ldap_entry(username)
      if res
        uu = User.create!(username: username,
                          name: res[:displayname][0],
                          last_name: res[:sn][0],
                          first_name: res[:givenname][0],
                          email: res[:mail][0])
      end
    end

    if uu.nil?
      return nil
    end
    section = CourseSection.find_by(crn: crn)
    if section.nil?
      return nil
    end
    # If creating the user fails, this will not create a registration
    # because there is a validation on user.
    registrations.where(user: uu)
                 .first_or_create(user_id: uu.id,
                                  course_id: self.id,
                                  section: section,
                                  role: role,
                                  show_in_lists: role == 'student')
  end

  def score_summary(for_students = nil)
    if for_students.nil?
      for_students = self.students
    end
    assns = self.assignments.where("available < ?", DateTime.current)
    open = assns.where("due_date > ?", DateTime.current)
    subs = UsedSub.where(user: for_students, assignment: assns)
      .joins(:submission)
      .select(:user_id, :assignment_id, :score)
      .to_a
    assns = assns.map{|a| [a.id, a]}.to_h
    avail = assns.reduce(0) do |tot, kv| tot + kv[1].points_available end
    total_points = self.assignments.reduce(0) do |tot, a| tot + a.points_available end
    # assume a default of 100 points, but there might be extra credit assignments that push the total above 100
    remaining = [100.0, total_points].max - avail
    ans = []
    self.users_with_drop_info(for_students).sort_by(&:sort_name).each do |s|
      dropped = s.dropped_date
      used = subs.select{|r| r.user_id == s.id}
      adjust = 0
      pending_names = []
      min = used.reduce(0.0) do |tot, sub| 
        if (assns[sub.assignment_id].points_available != 0)
          if sub.score.nil?
            adjust += assns[sub.assignment_id].points_available
            pending_names.push assns[sub.assignment_id].name
          end
          tot + ((sub.score || 0) * assns[sub.assignment_id].points_available / 100.0) 
        else
          tot
        end
      end
      cur = (100.0 * min) / (avail - adjust)
      max = min + remaining
      unsub_names = []
      unsubs = open.reduce(0.0) do |tot, o|
        if used.find{|u| u.assignment == o}.nil?
          unsub_names.push o.name
          tot + o.points_available
        else
          tot
        end
      end
      ans.push ({s: s, dropped: dropped, min: min, cur: cur, max: max,
                 pending: adjust, pending_names: pending_names,
                 unsub: unsubs, unsub_names: unsub_names,
                 remaining: remaining})
    end
    ans
  end
end
