class AddExtraCreditFields < ActiveRecord::Migration[5.1]
  def change
    add_column :assignments, :extra_credit, :boolean, null: false, default: false
    add_column :graders, :extra_credit, :boolean, null: false, default: false
  end
end
