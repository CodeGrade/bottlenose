class Interlock < ApplicationRecord
  belongs_to :assignment
  belongs_to :related_assignment, class_name: "Assignment"
  enum constraint: [:no_submission_unless_submitted,
                    :no_submission_after_viewing]
end
