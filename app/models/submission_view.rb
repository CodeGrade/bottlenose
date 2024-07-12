class SubmissionView < ApplicationRecord
  belongs_to :user
  belongs_to :assignment
  belongs_to :team, optional: true

  validates :user_id, presence: true
  validates :assignment_id, presence: true
end
