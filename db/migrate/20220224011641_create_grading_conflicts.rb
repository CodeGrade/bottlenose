class CreateGradingConflicts < ActiveRecord::Migration[5.2]
  def change
    create_table :grading_conflicts do |t|
      t.references :grader_user
      t.references :gradee_user
      t.references :course, foreign_key: true
      t.datetime :created_at
      t.datetime :updated_at
      t.timestamps
    end

    add_foreign_key :grading_conflicts, :users, column: :grader_user_id, primary_key: :id
    add_foreign_key :grading_conflicts, :users, column: :gradee_user_id, primary_key: :id

  end
end
