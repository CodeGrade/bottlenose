class UserSubmission < ActiveRecord::Base
  belongs_to :user
  belongs_to :submission

  def self.with_user(user)
    where(user_id: user)
  end

  def self.with_submission(sub)
    where(submission_id: sub)
  end
end
