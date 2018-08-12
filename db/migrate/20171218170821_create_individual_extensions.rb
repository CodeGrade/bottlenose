class CreateIndividualExtensions < ActiveRecord::Migration[5.1]
  def change
    create_table :individual_extensions do |t|
      t.integer "assignment_id", null: false
      t.integer "user_id"
      t.integer "team_id"
      t.datetime "due_date", null: false
      t.datetime "created_at"
      t.datetime "updated_at"
      t.index ["assignment_id", "user_id"], name: "unique_assn_user_extension", unique: true, where: "(team_id IS NULL)"
      t.index ["assignment_id", "team_id"], name: "unique_assn_team_extension", unique: true, where: "(user_id IS NULL)"
      t.index ["assignment_id"]
      t.index ["user_id"]
      t.index ["team_id"]
    end
  end
end
