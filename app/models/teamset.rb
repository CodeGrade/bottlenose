class Teamset < ActiveRecord::Base
  belongs_to :course
  has_many   :teams
  has_many   :submissions, through: :teams
  has_many   :assignments

  def to_s
    "Teamset #{self.id}"
  end
  
  def dup(revise_subs_for_assn = nil)
    # This method duplicates this team set, and duplicates all the
    # active teams associated with this teamset (assuming the relevant
    # students are still enrolled).  If an assignment is supplied,
    # then this updates the teams for that assignment's submissions to
    # refer to these newly-copied teams.  This method should only be
    # called with an assignment if that assignment's teamset is shared
    # with at least two assignments (otherwise, there's no need to
    # copy teams and modify assignments, anyway).
    
    # TODO: should this be in a transaction?
    new_ts = Teamset.new(course: self.course, name: "Copy of #{self.name}")
    new_ts.save!
    new_ts.copy_from(self, revise_subs_for_assn)
    return new_ts
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

  def dissolve_all(end_time = DateTime.current)
    active = self.active_teams
    count = active.count
    active.each do |t| t.dissolve(end_time) end
    return count
  end
  
  def users
    self.course.users_with_drop_info.order(:last_name, :first_name)
  end

  def active_teams
    self.teams.select(&:active?)
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
      team.save
      subs.update_all({team_id: team.id}) # does this need to be team_id, or can it be just team?
    end
  end
end
