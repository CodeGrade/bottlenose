class AddOrcaStatusToGrader < ActiveRecord::Migration[7.0]
  def change
    add_column :graders, :orca_status, :boolean, default: false, null: false
  end
end
