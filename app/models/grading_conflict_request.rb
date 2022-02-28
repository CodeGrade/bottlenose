class GradingConflictRequest < ApplicationRecord
  
  belongs_to :course
  belongs_to :user
  
  validates :course, presence: true
  validates :user, presence: true

  # TODO: Should return the Grader the student is trying to
  # suggest there is a conflict with.
  def conflicted_grader_user
  end

  def conflicted_user_is_grader
  end

end
