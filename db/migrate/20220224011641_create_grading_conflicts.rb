class CreateGradingConflicts < ActiveRecord::Migration[5.2]
  def change
    create_table :grading_conflicts do |t|
      t.references :grader_user, foreign_key: true
      t.references :gradee_user, foreign_key: true
      t.references :course, foreign_key: true
      t.datetime :created_at
      t.datetime :updated_at

      t.timestamps
    end
  end
end
