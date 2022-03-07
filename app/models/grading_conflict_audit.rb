class GradingConflictAudit < ApplicationRecord
  belongs_to :user
  belongs_to :grading_conflict
end