class CreateGradingConflicts < ActiveRecord::Migration[5.2]
  def change
    create_table :grading_conflicts do |t|
      t.references :staff, null: false
      t.references :student, null: false
      t.references :course, foreign_key: true, null: false
      t.integer :status, null: false, default: 0
      t.timestamps      
    end

    create_table :grading_conflict_audits do |t|
      t.references :grading_conflict, null: false, foreign_key: true
      t.references :user, null:false, foreign_key: true
      t.string :reason, null: true
      t.timestamps
      t.integer :status, null: false, default: 0
    end

    add_foreign_key :grading_conflicts, :users, column: :staff_id, primary_key: :id
    add_foreign_key :grading_conflicts, :users, column: :student_id, primary_key: :id
    add_index :grading_conflicts, [:staff_id, :student_id, :course_id], unique: true, name: "index_grading_conflict_uniqueness"

  end
end
