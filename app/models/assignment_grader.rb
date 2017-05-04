class AssignmentGrader < ApplicationRecord
  belongs_to :assignment
  belongs_to :grader
end
