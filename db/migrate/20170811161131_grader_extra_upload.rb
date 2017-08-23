class GraderExtraUpload < ActiveRecord::Migration[5.1]
  def change
    add_column :graders, :extra_upload_id, :integer
  end
end
