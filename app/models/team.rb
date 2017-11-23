class Team < ApplicationRecord
  belongs_to :course
  belongs_to :teamset
  has_many   :team_users, dependent: :destroy
  has_many   :users, through: :team_users
  has_many   :submissions

  validates :course_id,  presence: true
  validates :start_date, presence: true
  validates :users, presence: true
  validate :end_not_before_start
  validate :all_enrolled

  def to_s
    "Team #{self.id} - #{self.member_names}"
  end

  def member_names
    users.sort_by(&:sort_name).map(&:display_name).to_sentence
  end

  # If the end date of a team is not set (nil) then this team does not
  # have an end date, and as such will always be active. Start and end
  # dates form a half open interval. This means that the team with a
  # start date of 2016-02-05 and end date of 2016-02-10 was a team
  # active for only 5 days, and specifically not active on the 10th of
  # February.
  def active?
    if self.end_date
      Date.current.between?(self.start_date, self.end_date - 1)
    else
      true
    end
  end
  def self.active_query
    "((end_date IS NULL) OR (start_date <= ? AND ? < end_date))"
  end

  def dissolve(as_of)
    return unless self.end_date.nil? || (self.end_date > as_of)
    self.update_attribute(:end_date, as_of)
    stale_subs = self.submissions.joins(:assignment).where('due_date >= ?', as_of)
    stale_subs.update_all(stale_team: true)
    UsedSub.where(submission: stale_subs).delete_all
    CodereviewMatching.where(team: self).delete_all
    CodereviewMatching.where(target_team: self).delete_all
  end

  def self.users_for_teams(team_ids)
    team_subs = TeamUser.where(team_id: team_ids)
    user_ids = team_subs.select(:user_id)
    User.where(id: user_ids).distinct
  end

  def used_submissions
    Submission.joins("INNER JOIN used_subs ON submissions.id = used_subs.submission_id")
      .where(team: self)
  end

  private

  def all_enrolled
    not_enrolled = users.select {|u| u.registration_for(course).nil?}
    if not_enrolled.count > 0
      errors.add(:base, "The following team members are not enrolled in this course: " +
                        not_enrolled.to_a.map(&:username).to_sentence)
    end
  end

  def end_not_before_start
    return if end_date.blank?

    if end_date < start_date
      errors.add(:end_date, "must be not be before the start date")
    end
  end
end
