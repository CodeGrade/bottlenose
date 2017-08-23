class CodereviewMatching < ApplicationRecord
  belongs_to :assignment
  belongs_to :user
  belongs_to :team
  belongs_to :target_user, class_name: "User"
  belongs_to :target_team, class_name: "Team"

  validates :assignment, presence: true
  validate :some_src
  validate :some_target

  def some_src
    if self.user_id.nil? && self.team_id.nil?
      self.errors.add(:base, "Must specify a reviewer user or team")
    end
  end
  def some_target
    if self.target_user_id.nil? && self.target_team_id.nil?
      self.errors.add(:base, "Must specify a target user or team")
    end
  end
end
