class AddTimestampsToSubmissionViews < ActiveRecord::Migration[5.2]
  def change
    add_timestamps :submission_views, default: DateTime.now
  end
end
