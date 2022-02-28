class CreateGradingConflictRequests < ActiveRecord::Migration[5.2]
  def change
    create_table :grading_conflict_requests do |t|
      t.references :course, foreign_key: true
      t.references :user, foreign_key: true
      t.datetime :created_at

      t.timestamps
    end
  end
end
