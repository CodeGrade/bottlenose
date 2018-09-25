class IndexUsedSubsOnAssignmentId < ActiveRecord::Migration[5.2]
  def change
    add_index :used_subs, :assignment_id
  end
end
