class GradingConflict < ApplicationRecord

  belongs_to :grader_user, class_name: "User"
  belongs_to :gradee_user, class_name: "User"
  belongs_to :course

  # TODO: What else needs to be validated?
  validates :grader_user, presence: true
  validates :gradee_user, presence: true
  validates :course, presence: true
  validate :users_must_have_correct_roles


  def users_must_have_correct_roles
    unless self.users_have_proper_roles?
      errors.add(:base, "GradingConflict's grader_user is not a grader OR gradee_user is not a student.")
    end
  end

  # N/A -> Boolean 
  # Ensures that the :grader_user is a grader and the :gradee_user 
  # is a normal student
  def users_have_proper_roles?
    return (self.grader_user.course_grader?(course) || 
      self.grader_user.course_assistant?(course)) &&
      self.gradee_user.course_student?(course)
  end
end
