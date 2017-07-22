class CreateReviewFeedbacks < ActiveRecord::Migration[5.1]
  def change
    create_table :review_feedbacks do |t|
      t.integer "grade_id"
      t.integer "submission_id", null: false
      t.integer "review_submission_id", null: false
      t.integer "upload_id", null: false
      t.float "score"
      t.float "out_of"
      t.datetime "updated_at"
      t.index ["submission_id"], name: "index_review_feedbacks_on_submission_id"
      t.index ["review_submission_id"], name: "index_review_feedbacks_on_review_submission_id"
    end
    create_table :interlocks do |t|
      t.integer "assignment_id", null: false
      t.integer "related_assignment_id", null: false
      t.string "constraint", null: false
      t.index ["assignment_id"], name: "index_interlocks_on_assignment_id"
      t.index ["related_assignment_id"], name: "index_interlocks_on_related_assignment_id"
    end
  end
end
