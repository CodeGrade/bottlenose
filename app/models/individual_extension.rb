class IndividualExtension < ApplicationRecord
  belongs_to :user
  belongs_to :team
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
  
  private
  def user_or_team
    if user.nil? && team.nil?
      errors.add(:base, "Either user or team must be set")
    elsif user && team
      errors.add(:base, "Exactly one of user or team must be set")
    end
  end
end
