class IndexInlineCommentsOnSeverity < ActiveRecord::Migration[5.1]
  def change
    add_index :inline_comments, [:severity]
  end
end
