class GraderAllocation < ActiveRecord::Base
  belongs_to :assignment
  belongs_to :course
  belongs_to :submission
  has_one :grader, class_name: "User", :primary_key => "grader_id", :foreign_key => "id"
end
