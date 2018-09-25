class CreateSubmissionEnabledToggles < ActiveRecord::Migration[5.2]
  def change
    create_table :submission_enabled_toggles do |t|
      t.integer :section_id, null: false
      t.integer :assignment_id, null: false
      t.boolean :submissions_allowed, default: false

      t.timestamps
    end
  end
end
