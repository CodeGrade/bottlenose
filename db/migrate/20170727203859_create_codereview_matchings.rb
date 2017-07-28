class CreateCodereviewMatchings < ActiveRecord::Migration[5.1]
  def change
    create_table :codereview_matchings do |t|
      t.integer :assignment_id, null: false
      t.integer :user_id
      t.integer :team_id
      t.integer :target_user_id
      t.integer :target_team_id
      t.index [:user_id]
      t.index [:team_id]
    end
  end
end
