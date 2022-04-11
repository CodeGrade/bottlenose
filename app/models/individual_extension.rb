class IndividualExtension < ApplicationRecord
  belongs_to :user, optional: true
  belongs_to :team, optional: true
  belongs_to :assignment

  validate :user_or_team

  before_destroy do |ext|
    # Find any submissions that might've been impacted by this extension,
    # which have already been graded, and recompute their final scores
    if ext.user
      subs = ext.assignment.all_used_subs.where(user: ext.user).where.not(score: nil)
    else
      subs = ext.assignment.all_used_subs.where(team: ext.team).where.not(score: nil)
    end
    subs.each{|s| s.compute_grade!}
  end

  def to_s
    "User #{self.user_id || '<nil>'}/Team #{self.team_id || '<nil>'} can work on assignment #{self.assignment_id} until #{self.due_date} (instead of #{self.assignment.due_date})"
  end
  
  private
  def user_or_team
    if user.nil? && team.nil?
      errors.add(:base, "Either user or team must be set")
    elsif user && team
      errors.add(:base, "Exactly one of user or team must be set")
    end
  end
end
