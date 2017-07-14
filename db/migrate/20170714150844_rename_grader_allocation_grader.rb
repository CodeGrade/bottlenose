class RenameGraderAllocationGrader < ActiveRecord::Migration[5.1]
  def change
    change_table :grader_allocations do |t|
      t.rename :grade_id, :who_grades_id
    end    
  end
end
