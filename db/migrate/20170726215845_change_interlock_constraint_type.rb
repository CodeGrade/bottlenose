class ChangeInterlockConstraintType < ActiveRecord::Migration[5.1]
  def up
    remove_column :interlocks, :constraint
    add_column :interlocks, :constraint, :integer, null: false
  end
  def down
    change_column :interlocks, :constraint, :string
  end
end
