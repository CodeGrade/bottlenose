class RenameTeamSets < ActiveRecord::Migration[5.1]
  def change
    rename_table :team_sets, :teamsets
    rename_column :teams, :team_set_id, :teamset_id
    rename_column :assignments, :team_set_id, :teamset_id
  end
end
