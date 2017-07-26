# This file is auto-generated from the current state of the database. Instead
# of editing this file, please use the migrations feature of Active Record to
# incrementally modify your database, and then regenerate this schema definition.
#
# Note that this schema.rb definition is the authoritative source for your
# database schema. If you need to create the application database on another
# system, you should be using db:schema:load, not running all the migrations
# from scratch. The latter is a flawed and unsustainable approach (the more migrations
# you'll amass, the slower it'll run and the greater likelihood for issues).
#
# It's strongly recommended that you check this file into your version control system.

ActiveRecord::Schema.define(version: 20170726231116) do

  # These are extensions that must be enabled in order to support this database
  enable_extension "plpgsql"

  create_table "assignments", id: :serial, force: :cascade do |t|
    t.string "name", null: false
    t.datetime "due_date", null: false
    t.string "assignment_file_name"
    t.text "assignment"
    t.datetime "created_at"
    t.datetime "updated_at"
    t.string "secret_dir"
    t.boolean "hide_grading", default: false
    t.integer "assignment_upload_id"
    t.integer "blame_id"
    t.string "tar_key"
    t.integer "course_id", null: false
    t.boolean "team_subs"
    t.integer "max_attempts"
    t.integer "rate_per_hour"
    t.float "points_available"
    t.integer "lateness_config_id"
    t.datetime "available", null: false
    t.string "type", null: false
    t.integer "related_assignment_id"
    t.boolean "request_time_taken", default: false
    t.integer "teamset_id", null: false
    t.index ["course_id"], name: "index_assignments_on_course_id"
  end

  create_table "courses", id: :serial, force: :cascade do |t|
    t.string "name", null: false
    t.datetime "created_at"
    t.datetime "updated_at"
    t.text "footer"
    t.integer "term_id"
    t.integer "sub_max_size", default: 5, null: false
    t.boolean "public", default: false, null: false
    t.integer "team_min"
    t.integer "team_max"
    t.integer "total_late_days"
    t.integer "lateness_config_id", default: 0, null: false
  end

  create_table "delayed_jobs", id: :serial, force: :cascade do |t|
    t.integer "priority", default: 0, null: false
    t.integer "attempts", default: 0, null: false
    t.text "handler", null: false
    t.text "last_error"
    t.datetime "run_at"
    t.datetime "locked_at"
    t.datetime "failed_at"
    t.string "locked_by"
    t.string "queue"
    t.datetime "created_at"
    t.datetime "updated_at"
    t.index ["priority", "run_at"], name: "delayed_jobs_priority"
  end

  create_table "grader_allocations", id: :serial, force: :cascade do |t|
    t.integer "course_id", null: false
    t.integer "assignment_id", null: false
    t.integer "submission_id", null: false
    t.integer "who_grades_id", null: false
    t.datetime "grading_assigned", null: false
    t.boolean "abandoned", default: false, null: false
    t.datetime "grading_completed"
    t.index ["assignment_id"], name: "index_grader_allocations_on_assignment_id"
    t.index ["course_id"], name: "index_grader_allocations_on_course_id"
    t.index ["submission_id"], name: "index_grader_allocations_on_submission_id"
    t.index ["who_grades_id"], name: "index_grader_allocations_on_who_grades_id"
  end

  create_table "graders", id: :serial, force: :cascade do |t|
    t.string "type"
    t.float "avail_score"
    t.string "params"
    t.integer "upload_id"
    t.integer "order", null: false
    t.integer "assignment_id", null: false
  end

  create_table "grades", id: :serial, force: :cascade do |t|
    t.integer "grader_id", null: false
    t.integer "submission_id", null: false
    t.string "grading_output"
    t.text "notes"
    t.float "score"
    t.float "out_of"
    t.datetime "updated_at"
    t.boolean "available", default: false
    t.index ["submission_id"], name: "index_grades_on_submission_id"
  end

  create_table "inline_comments", id: :serial, force: :cascade do |t|
    t.integer "submission_id", null: false
    t.string "title", null: false
    t.string "filename", null: false
    t.integer "line", null: false
    t.integer "grade_id", null: false
    t.integer "user_id"
    t.string "label", null: false
    t.integer "severity", null: false
    t.string "comment", default: "", null: false
    t.float "weight", null: false
    t.boolean "suppressed", default: false, null: false
    t.string "info"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.index ["filename"], name: "index_inline_comments_on_filename"
    t.index ["label"], name: "index_inline_comments_on_label"
    t.index ["submission_id", "grade_id", "line"], name: "index_inline_comments_on_submission_id_and_grade_id_and_line"
  end

  create_table "interlocks", force: :cascade do |t|
    t.integer "assignment_id", null: false
    t.integer "related_assignment_id", null: false
    t.integer "constraint", null: false
    t.index ["assignment_id"], name: "index_interlocks_on_assignment_id"
    t.index ["related_assignment_id"], name: "index_interlocks_on_related_assignment_id"
  end

  create_table "lateness_configs", id: :serial, force: :cascade do |t|
    t.string "type"
    t.integer "days_per_assignment"
    t.integer "percent_off"
    t.integer "frequency"
    t.integer "max_penalty"
  end

  create_table "reg_request_sections", force: :cascade do |t|
    t.integer "reg_request_id", null: false
    t.integer "section_id", null: false
    t.index ["reg_request_id"], name: "index_reg_request_sections_on_reg_request_id"
    t.index ["section_id"], name: "index_reg_request_sections_on_section_id"
  end

  create_table "reg_requests", id: :serial, force: :cascade do |t|
    t.integer "course_id"
    t.text "notes"
    t.datetime "created_at"
    t.datetime "updated_at"
    t.integer "user_id"
    t.integer "role", default: 0, null: false
  end

  create_table "registration_sections", force: :cascade do |t|
    t.integer "registration_id", null: false
    t.integer "section_id", null: false
    t.index ["registration_id"], name: "index_registration_sections_on_registration_id"
    t.index ["section_id"], name: "index_registration_sections_on_section_id"
  end

  create_table "registrations", id: :serial, force: :cascade do |t|
    t.integer "course_id", null: false
    t.integer "user_id", null: false
    t.datetime "created_at"
    t.datetime "updated_at"
    t.boolean "show_in_lists"
    t.string "tags", default: ""
    t.integer "role", default: 0, null: false
    t.datetime "dropped_date"
    t.index ["course_id"], name: "index_registrations_on_course_id"
    t.index ["user_id"], name: "index_registrations_on_user_id"
  end

  create_table "review_feedbacks", force: :cascade do |t|
    t.integer "grade_id"
    t.integer "submission_id", null: false
    t.integer "review_submission_id", null: false
    t.integer "upload_id", null: false
    t.float "score"
    t.float "out_of"
    t.datetime "updated_at"
    t.index ["review_submission_id"], name: "index_review_feedbacks_on_review_submission_id"
    t.index ["submission_id"], name: "index_review_feedbacks_on_submission_id"
  end

  create_table "sandboxes", id: :serial, force: :cascade do |t|
    t.string "name"
    t.integer "submission_id"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
  end

  create_table "sections", id: :serial, force: :cascade do |t|
    t.integer "course_id", null: false
    t.integer "crn", null: false
    t.string "meeting_time"
    t.integer "instructor_id", null: false
    t.integer "type", default: 0, null: false
    t.index ["course_id"], name: "index_sections_on_course_id"
    t.index ["crn"], name: "index_sections_on_crn", unique: true
  end

  create_table "submission_views", force: :cascade do |t|
    t.integer "user_id", null: false
    t.integer "team_id"
    t.integer "assignment_id", null: false
  end

  create_table "submissions", id: :serial, force: :cascade do |t|
    t.integer "assignment_id", null: false
    t.integer "user_id", null: false
    t.string "secret_dir"
    t.string "file_name"
    t.text "student_notes"
    t.datetime "created_at"
    t.datetime "updated_at"
    t.boolean "ignore_late_penalty", default: false
    t.integer "upload_id"
    t.integer "upload_size", default: 0, null: false
    t.integer "team_id"
    t.integer "comments_upload_id"
    t.boolean "stale_team"
    t.float "score"
    t.string "type", null: false
    t.float "time_taken"
    t.index ["assignment_id"], name: "index_submissions_on_assignment_id"
    t.index ["user_id", "assignment_id"], name: "index_submissions_on_user_id_and_assignment_id"
    t.index ["user_id"], name: "index_submissions_on_user_id"
  end

  create_table "team_users", id: :serial, force: :cascade do |t|
    t.integer "team_id"
    t.integer "user_id"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.index ["team_id", "user_id"], name: "unique_team_memebers", unique: true
  end

  create_table "teams", id: :serial, force: :cascade do |t|
    t.integer "course_id"
    t.date "start_date"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.date "end_date"
    t.integer "teamset_id", null: false
  end

  create_table "teamsets", id: :serial, force: :cascade do |t|
    t.integer "course_id", null: false
    t.string "name"
    t.index ["course_id"], name: "index_teamsets_on_course_id"
  end

  create_table "terms", id: :serial, force: :cascade do |t|
    t.string "name"
    t.boolean "archived", default: false
    t.datetime "created_at"
    t.datetime "updated_at"
  end

  create_table "uploads", id: :serial, force: :cascade do |t|
    t.integer "user_id"
    t.string "file_name"
    t.string "secret_key"
    t.datetime "created_at"
    t.datetime "updated_at"
    t.index ["secret_key"], name: "index_uploads_on_secret_key", unique: true
  end

  create_table "used_subs", id: :serial, force: :cascade do |t|
    t.integer "user_id", null: false
    t.integer "assignment_id", null: false
    t.integer "submission_id", null: false
    t.index ["user_id", "assignment_id"], name: "unique_sub_per_user_assignment", unique: true
  end

  create_table "user_submissions", id: :serial, force: :cascade do |t|
    t.integer "user_id"
    t.integer "submission_id"
    t.index ["submission_id"], name: "index_user_submissions_on_submission_id"
    t.index ["user_id", "submission_id"], name: "index_user_submissions_on_user_id_and_submission_id", unique: true
    t.index ["user_id"], name: "index_user_submissions_on_user_id"
  end

  create_table "users", id: :serial, force: :cascade do |t|
    t.string "name", null: false
    t.string "email"
    t.boolean "site_admin"
    t.datetime "created_at"
    t.datetime "updated_at"
    t.string "encrypted_password", default: "", null: false
    t.string "reset_password_token"
    t.datetime "reset_password_sent_at"
    t.datetime "remember_created_at"
    t.integer "sign_in_count", default: 0, null: false
    t.datetime "current_sign_in_at"
    t.datetime "last_sign_in_at"
    t.inet "current_sign_in_ip"
    t.inet "last_sign_in_ip"
    t.string "username"
    t.text "first_name"
    t.text "last_name"
    t.text "nickname"
    t.text "profile"
    t.integer "nuid"
    t.index ["email"], name: "index_users_on_email", unique: true
    t.index ["nuid"], name: "index_users_on_nuid", unique: true
    t.index ["reset_password_token"], name: "index_users_on_reset_password_token", unique: true
    t.index ["username"], name: "index_users_on_username", unique: true
  end

end
