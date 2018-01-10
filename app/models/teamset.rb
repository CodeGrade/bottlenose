class Teamset < ActiveRecord::Base
  belongs_to :course
  has_many   :teams
  has_many   :submissions, through: :teams
  has_many   :assignments
  has_many   :team_requests

  attr_accessor :bulk_teams
  
  def to_s
    "Teamset #{self.id}"
  end
  
  def dup(revise_subs_for_assn = nil, name = nil)
    # This method duplicates this team set, and duplicates all the
    # active teams associated with this teamset (assuming the relevant
    # students are still enrolled).  If an assignment is supplied,
    # then this updates the teams for that assignment's submissions to
    # refer to these newly-copied teams.  This method should only be
    # called with an assignment if that assignment's teamset is shared
    # with at least two assignments (otherwise, there's no need to
    # copy teams and modify assignments, anyway).
    
    # TODO: should this be in a transaction?
    new_ts = Teamset.new(course: self.course, name: name || "Copy of #{self.name}")
    new_ts.save!
    new_ts.copy_from(self, revise_subs_for_assn)
    return new_ts
  end

  def randomize(size, within_section, start_date, end_date = nil)
    # This method randomly allocates the un-teamed students in this teamset
    # into new teams of the given size
    count = 0
    case within_section
    when "course"
      grouped_students_to_team = [students_without_active_team]
    else
      unteamed_students = students_without_active_team
      reg_sections = RegistrationSection.where(registration_id: unteamed_students.map(&:reg_id)).group_by(&:section_id)
      sections = self.course.sections.group_by(&:type)
      students_by_reg = unteamed_students.map{|s| [s.reg_id, s]}.to_h
      grouped_students_to_team = sections[within_section].map do |s|
        students_by_reg.values_at(*reg_sections[s.id]&.map(&:registration_id))
      end
    end
    leftovers = []
    grouped_students_to_team.each do |students_to_team|
      students_to_team.shuffle!.each_slice(size).each do |t|
        @team = Team.new(course: self.course,
                         start_date: start_date,
                         end_date: end_date,
                         teamset: self
                        )

        if t.count == size
          @team.users = t
          
          if @team.save
            count += 1
          else
            debugger
          end
        else
          leftovers += t
        end
      end
    end
    return count
  end

  def copy_from(src, revise_subs_for_assn)
    self.dissolve_all
    count = 0
    dropped_users = src.users.pluck(:user_id, :dropped_date).to_h
    src.active_teams.each do |team|
      if team.users.any?{|u| dropped_users[u.id]}
        debugger
        next
      end
      new_team = team.dup
      new_team.users = team.users
      new_team.teamset = self
      new_team.save!
      if revise_subs_for_assn
        team.submissions
          .where(assignment: revise_subs_for_assn)
          .update_all({team_id: new_team.id}) # does this need to be team_id, or can it be just team?
      end
      count += 1
    end
    return count
  end

  def dissolve_all(end_time = DateTime.current, section_id=nil)
    if section_id
      active = self.active_teams_in_section(section_id)
    else
      active = self.active_teams
    end
    count = active.count
    active.each do |t| t.dissolve(end_time) end
    return count
  end
  
  def users
    self.course.users_with_drop_info.order(:last_name, :first_name)
  end

  def active_teams
    self.teams.where(Team.active_query, Date.current, Date.current).includes(:users)
  end

  def active_teams_in_section(section_id)
    section = course.sections.find(section_id)
    return [] unless section
    users_in_section = section.users.map{|u| [u.id, true]}.to_h
    self.active_teams.select do |team|
      team.users.any?{|u| users_in_section[u.id]}
    end
  end

  def active_team_for(user)
    user.teams.where(course: self.course, teamset: self).select(&:active?).first
  end

  def active_teams_for(users)
    self.active_teams.map do |t|
      t.users.map{|u| [u.id, t]}
    end.flatten(1).to_h.slice(*users.map(&:id))
  end

  def students_without_active_team
    users_without_active_team(self.course.students)
  end

  def users_without_active_team(for_users = nil)
    for_users = self.course.users unless for_users
    @uwdi = self.course.users_with_drop_info(for_users)
    @relevant_teams = Team
                      .joins(:team_users)
                      .select("teams.*", "team_users.user_id as uid")
                      .where("team_users.user_id IN (?)", @uwdi.pluck(:id))
                      .group_by(&:uid)
    @uwdi.where("registrations.dropped_date": nil).reject do |student|
      @relevant_teams[student.id]&.any? do |t| t.teamset_id == self.id && t.active? end
    end
  end

  def make_solo_teams_for(assn)
    # This creates singleton teams for everyone who's submitted to the assignment already,
    # and updates all those submissions to point to these new teams.  This method should be used
    # only when converting an assignment from non-teams to team submission, but just in case,
    # it only modifies submissions that don't already have a team.
    assn.submissions.where(team: nil).group_by(&:user_id).each do |uid, subs|
      team = Team.new(course: self.course,
                      start_date: assn.available,
                      end_date: Date.today,
                      teamset: self)
      team.users = [User.find(uid)]
      team.save
      Submission.where(id: subs.map(&:id)).update_all({team_id: team.id})
      # does this need to be team_id, or can it be just team?
    end
  end
end
