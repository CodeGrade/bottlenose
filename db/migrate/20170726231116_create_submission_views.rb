class CreateSubmissionViews < ActiveRecord::Migration[5.1]
  def change
    create_table :submission_views do |t|
      t.integer :user_id, null: false
      t.integer :team_id, null: true # teams might be optional
      t.integer :assignment_id, null: false
    end
  end
end
