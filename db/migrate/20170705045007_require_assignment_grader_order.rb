class RequireAssignmentGraderOrder < ActiveRecord::Migration[5.1]
  def change
    change_column_null :assignment_graders, :order, false
  end
end
