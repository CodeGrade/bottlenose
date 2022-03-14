class GradingConflictAudit < ApplicationRecord
  enum status: [:active, :inactive, :pending, :rejected]
  belongs_to :user
  belongs_to :grading_conflict
end