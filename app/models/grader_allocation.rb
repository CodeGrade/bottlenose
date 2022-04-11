class GraderAllocation < ApplicationRecord
  belongs_to :assignment
  belongs_to :course
  belongs_to :submission
  has_one :who_grades, class_name: "User", :primary_key => "who_grades_id", :foreign_key => "id"

  def conflict_currently_exists?
    GradingConflict.exists?(course: self.course, 
      staff_id: self.who_grades_id, 
      student: submission.users, 
      status: :active)
  end
end
