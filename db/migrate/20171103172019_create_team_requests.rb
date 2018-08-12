class CreateTeamRequests < ActiveRecord::Migration[5.1]
  def change
    create_table :team_requests do |t|
      t.integer "teamset_id", null: false
      t.integer "user_id", null: false
      t.string "partner_names", null: false
      t.index :teamset_id
      t.index :user_id
      t.datetime "created_at"
      t.datetime "updated_at"
      t.index [:teamset_id, :user_id], unique: true
    end
  end
end
