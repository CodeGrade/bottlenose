class IndexCodereviewMatchingsOnAssignmentId < ActiveRecord::Migration[5.1]
  def change
    add_index "codereview_matchings", ["assignment_id"], name: "index_codereview_matchings_on_assignment_id"
  end
end
