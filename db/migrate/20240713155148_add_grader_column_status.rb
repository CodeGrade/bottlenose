class AddGraderColumnStatus < ActiveRecord::Migration[7.0]
  def change
    add_column :graders, :status, :json, default: { useOrca: false }
  end
end
