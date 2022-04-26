class ChangePrimaryKeyToBigint < ActiveRecord::Migration[5.2]
  def change
    change_column :assignments, :id, :bigint
    change_column :courses, :id, :bigint
    change_column :grader_allocations, :id, :bigint
    change_column :graders, :id, :bigint
    change_column :grades, :id, :bigint
    change_column :inline_comments, :id, :bigint
    change_column :lateness_configs, :id, :bigint
    change_column :reg_requests, :id, :bigint
    change_column :registrations, :id, :bigint
    change_column :sandboxes, :id, :bigint
    change_column :sections, :id, :bigint
    change_column :submissions, :id, :bigint
    change_column :team_users, :id, :bigint
    change_column :teams, :id, :bigint
    change_column :teamsets, :id, :bigint
    change_column :terms, :id, :bigint
    change_column :uploads, :id, :bigint
    change_column :used_subs, :id, :bigint
    change_column :user_submissions, :id, :bigint
    change_column :users, :id, :bigint
  end
end
