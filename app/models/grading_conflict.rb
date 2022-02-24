class GradingConflict < ApplicationRecord

  # TODO: Need to add validations to ensure grader_user is a grader (check with course)
  # TODO: Need to add validation to ensure gradee_user is a student (check role with course)
  # TODO: What else needs to be validated?

  belongs_to :grader_user
  belongs_to :gradee_user
  belongs_to :course
end
