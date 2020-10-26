class AddUserIndexToInlineComments < ActiveRecord::Migration[5.2]
  def change
    add_index "inline_comments", ["user_id"], name: "index_inline_comments_on_user_id", using: :btree
  end
end
