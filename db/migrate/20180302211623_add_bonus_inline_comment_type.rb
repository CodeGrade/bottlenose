class AddBonusInlineCommentType < ActiveRecord::Migration[5.1]
  def up
    InlineComment.where(severity: "info").where("weight < 0").each do |c|
      c.update(severity: "bonus", weight: 0 - c.weight)
    end
  end
  def down
    InlineComment.where(severity: "bonus").each do |c|
      c.update(severity: "info", weight: 0 - c.weight)
    end
  end
end
