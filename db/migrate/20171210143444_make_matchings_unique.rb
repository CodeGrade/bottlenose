class MakeMatchingsUnique < ActiveRecord::Migration[5.1]
  def change
    add_index "codereview_matchings", [:assignment_id, :user_id, :target_user_id],
              where: "(team_id IS NULL AND target_team_id IS NULL)",
              unique: true, name: "unique_user_user_matchings"
    add_index "codereview_matchings", [:assignment_id, :user_id, :target_team_id],
              where: "(team_id IS NULL AND target_user_id IS NULL)",
              unique: true, name: "unique_user_team_matchings"
    add_index "codereview_matchings", [:assignment_id, :team_id, :target_user_id],
              where: "(user_id IS NULL AND target_team_id IS NULL)",
              unique: true, name: "unique_team_user_matchings"
    add_index "codereview_matchings", [:assignment_id, :team_id, :target_team_id],
              where: "(user_id IS NULL AND target_user_id IS NULL)",
              unique: true, name: "unique_team_team_matchings"
  end
end
