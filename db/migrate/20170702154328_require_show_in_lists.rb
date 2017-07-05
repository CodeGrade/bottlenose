class RequireShowInLists < ActiveRecord::Migration[5.1]
  def change
    change_column_null :registrations, :show_in_lists, true
  end
end
