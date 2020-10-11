class AddTimestampsToSubmissionViews < ActiveRecord::Migration[5.2]
  def change
    add_timestamps :submission_views, null: true

    timestamp = DateTime.new(2020, 10, 07, 11, 50, 16)
    SubmissionView.update_all(created_at: timestamp, updated_at: timestamp)

    change_column_null :submission_views, :created_at, false
    change_column_null :submission_views, :updated_at, false
  end
end
