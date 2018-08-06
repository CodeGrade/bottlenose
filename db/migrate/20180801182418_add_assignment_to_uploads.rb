class AddAssignmentToUploads < ActiveRecord::Migration[5.2]
  def up
    add_column :uploads, :assignment_id, :integer, default: 0, null: false
    auids = Assignment.all.group_by(&:assignment_upload_id)
    suids = Submission.all.group_by(&:upload_id)
    cuids = Submission.all.group_by(&:comments_upload_id)
    guids = Grader.all.group_by(&:upload_id)

    Upload.all.each do |u|
      a = auids[u.id]
      s = suids[u.id]
      c = cuids[u.id]
      g = guids[u.id]
      if !a.blank?
        u.assignment_id = a[0].id
      elsif !s.blank?
        u.assignment_id = s[0].assignment_id
      elsif !c.blank?
        u.assignment_id = c[0].assignment_id
      elsif !g.blank?
        u.assignment_id = g[0].assignment_id
      end
      u.save
    end
    add_index :uploads, [:assignment_id], unique: false
  end
  def down
    remove_column :uploads, :assignment_id
  end
end
