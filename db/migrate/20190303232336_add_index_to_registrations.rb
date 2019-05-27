class AddIndexToRegistrations < ActiveRecord::Migration[5.2]
  def change
    add_index :registrations, [:course_id, :user_id], unique: true
  end
end
