class IndexInlineComments < ActiveRecord::Migration[5.1]
  def change
    add_index "inline_comments", [:grade_id]
    add_index "inline_comments", [:submission_id]
  end
end
