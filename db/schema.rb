# This file is auto-generated from the current state of the database. Instead
# of editing this file, please use the migrations feature of Active Record to
# incrementally modify your database, and then regenerate this schema definition.
#
# This file is the source Rails uses to define your schema when running `bin/rails
# db:schema:load`. When creating a new database, `bin/rails db:schema:load` tends to
# be faster and is potentially less error prone than running all of your
# migrations from scratch. Old migrations may fail to apply correctly if those
# migrations use external dependencies or application code.
#
# It's strongly recommended that you check this file into your version control system.

ActiveRecord::Schema[7.0].define(version: 2024_07_25_235205) do
  # These are extensions that must be enabled in order to support this database
  enable_extension "plpgsql"

  create_table "assignments", force: :cascade do |t|
    t.string "name", null: false
    t.datetime "due_date", precision: nil, null: false
    t.string "assignment_file_name"
    t.text "assignment"
    t.datetime "created_at", precision: nil
    t.datetime "updated_at", precision: nil
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
    t.datetime "available", precision: nil, null: false
    t.string "type", null: false
    t.integer "related_assignment_id"
    t.boolean "request_time_taken", default: false
    t.integer "teamset_id", null: false
    t.boolean "extra_credit", default: false, null: false
    t.index ["course_id"], name: "index_assignments_on_course_id"
  end

  create_table "codereview_matchings", force: :cascade do |t|
    t.integer "assignment_id", null: false
    t.integer "user_id"
    t.integer "team_id"
    t.integer "target_user_id"
    t.integer "target_team_id"
    t.index ["assignment_id", "team_id", "target_team_id"], name: "unique_team_team_matchings", unique: true, where: "((user_id IS NULL) AND (target_user_id IS NULL))"
    t.index ["assignment_id", "team_id", "target_user_id"], name: "unique_team_user_matchings", unique: true, where: "((user_id IS NULL) AND (target_team_id IS NULL))"
    t.index ["assignment_id", "user_id", "target_team_id"], name: "unique_user_team_matchings", unique: true, where: "((team_id IS NULL) AND (target_user_id IS NULL))"
    t.index ["assignment_id", "user_id", "target_user_id"], name: "unique_user_user_matchings", unique: true, where: "((team_id IS NULL) AND (target_team_id IS NULL))"
    t.index ["assignment_id"], name: "index_codereview_matchings_on_assignment_id"
    t.index ["team_id"], name: "index_codereview_matchings_on_team_id"
    t.index ["user_id"], name: "index_codereview_matchings_on_user_id"
  end

  create_table "courses", force: :cascade do |t|
    t.string "name", null: false
    t.datetime "created_at", precision: nil
    t.datetime "updated_at", precision: nil
    t.text "footer"
    t.integer "term_id"
    t.integer "sub_max_size", default: 5, null: false
    t.boolean "public", default: false, null: false
    t.integer "team_min"
    t.integer "team_max"
    t.integer "total_late_days"
    t.integer "lateness_config_id", default: 0, null: false
  end

  create_table "grader_allocations", force: :cascade do |t|
    t.integer "course_id", null: false
    t.integer "assignment_id", null: false
    t.integer "submission_id", null: false
    t.integer "who_grades_id", null: false
    t.datetime "grading_assigned", precision: nil, null: false
    t.boolean "abandoned", default: false, null: false
    t.datetime "grading_completed", precision: nil
    t.index ["assignment_id"], name: "index_grader_allocations_on_assignment_id"
    t.index ["course_id"], name: "index_grader_allocations_on_course_id"
    t.index ["submission_id"], name: "index_grader_allocations_on_submission_id"
    t.index ["who_grades_id"], name: "index_grader_allocations_on_who_grades_id"
  end

  create_table "graders", force: :cascade do |t|
    t.string "type"
    t.float "avail_score"
    t.string "params"
    t.integer "upload_id"
    t.integer "order", null: false
    t.integer "assignment_id", null: false
    t.integer "extra_upload_id"
    t.boolean "extra_credit", default: false, null: false
    t.boolean "orca_status", default: false, null: false
  end

  create_table "grades", force: :cascade do |t|
    t.integer "grader_id", null: false
    t.integer "submission_id", null: false
    t.string "grading_output"
    t.text "notes"
    t.float "score"
    t.float "out_of"
    t.datetime "updated_at", precision: nil
    t.boolean "available", default: false
    t.index ["submission_id"], name: "index_grades_on_submission_id"
  end

  create_table "grading_conflict_audits", force: :cascade do |t|
    t.bigint "grading_conflict_id", null: false
    t.bigint "user_id", null: false
    t.string "reason"
    t.datetime "created_at", precision: nil, null: false
    t.datetime "updated_at", precision: nil, null: false
    t.integer "status", default: 0, null: false
    t.index ["grading_conflict_id"], name: "index_grading_conflict_audits_on_grading_conflict_id"
    t.index ["user_id"], name: "index_grading_conflict_audits_on_user_id"
  end

  create_table "grading_conflicts", force: :cascade do |t|
    t.bigint "staff_id", null: false
    t.bigint "student_id", null: false
    t.bigint "course_id", null: false
    t.integer "status", default: 0, null: false
    t.datetime "created_at", precision: nil, null: false
    t.datetime "updated_at", precision: nil, null: false
    t.index ["course_id"], name: "index_grading_conflicts_on_course_id"
    t.index ["staff_id", "student_id", "course_id"], name: "index_grading_conflict_uniqueness", unique: true
    t.index ["staff_id"], name: "index_grading_conflicts_on_staff_id"
    t.index ["student_id"], name: "index_grading_conflicts_on_student_id"
  end

  create_table "individual_extensions", force: :cascade do |t|
    t.integer "assignment_id", null: false
    t.integer "user_id"
    t.integer "team_id"
    t.datetime "due_date", precision: nil, null: false
    t.datetime "created_at", precision: nil
    t.datetime "updated_at", precision: nil
    t.index ["assignment_id", "team_id"], name: "unique_assn_team_extension", unique: true, where: "(user_id IS NULL)"
    t.index ["assignment_id", "user_id"], name: "unique_assn_user_extension", unique: true, where: "(team_id IS NULL)"
    t.index ["assignment_id"], name: "index_individual_extensions_on_assignment_id"
    t.index ["team_id"], name: "index_individual_extensions_on_team_id"
    t.index ["user_id"], name: "index_individual_extensions_on_user_id"
  end

  create_table "inline_comments", force: :cascade do |t|
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
    t.datetime "created_at", precision: nil, null: false
    t.datetime "updated_at", precision: nil, null: false
    t.index ["filename"], name: "index_inline_comments_on_filename"
    t.index ["grade_id"], name: "index_inline_comments_on_grade_id"
    t.index ["label"], name: "index_inline_comments_on_label"
    t.index ["severity"], name: "index_inline_comments_on_severity"
    t.index ["submission_id", "grade_id", "line"], name: "index_inline_comments_on_submission_id_and_grade_id_and_line"
    t.index ["submission_id"], name: "index_inline_comments_on_submission_id"
    t.index ["user_id"], name: "index_inline_comments_on_user_id"
  end

  create_table "interlocks", force: :cascade do |t|
    t.integer "assignment_id", null: false
    t.integer "related_assignment_id", null: false
    t.integer "constraint", null: false
    t.index ["assignment_id"], name: "index_interlocks_on_assignment_id"
    t.index ["related_assignment_id"], name: "index_interlocks_on_related_assignment_id"
  end

  create_table "lateness_configs", force: :cascade do |t|
    t.string "type"
    t.integer "days_per_assignment"
    t.integer "percent_off"
    t.integer "frequency"
    t.integer "max_penalty"
  end

  create_table "oauth_access_grants", force: :cascade do |t|
    t.bigint "resource_owner_id", null: false
    t.bigint "application_id", null: false
    t.string "token", null: false
    t.integer "expires_in", null: false
    t.text "redirect_uri", null: false
    t.datetime "created_at", precision: nil, null: false
    t.datetime "revoked_at", precision: nil
    t.string "scopes", default: "", null: false
    t.index ["application_id"], name: "index_oauth_access_grants_on_application_id"
    t.index ["resource_owner_id"], name: "index_oauth_access_grants_on_resource_owner_id"
    t.index ["token"], name: "index_oauth_access_grants_on_token", unique: true
  end

  create_table "oauth_access_tokens", force: :cascade do |t|
    t.bigint "resource_owner_id"
    t.bigint "application_id", null: false
    t.string "token", null: false
    t.string "refresh_token"
    t.integer "expires_in"
    t.datetime "revoked_at", precision: nil
    t.datetime "created_at", precision: nil, null: false
    t.string "scopes"
    t.string "previous_refresh_token", default: "", null: false
    t.index ["application_id"], name: "index_oauth_access_tokens_on_application_id"
    t.index ["refresh_token"], name: "index_oauth_access_tokens_on_refresh_token", unique: true
    t.index ["resource_owner_id"], name: "index_oauth_access_tokens_on_resource_owner_id"
    t.index ["token"], name: "index_oauth_access_tokens_on_token", unique: true
  end

  create_table "oauth_applications", force: :cascade do |t|
    t.string "name", null: false
    t.string "uid", null: false
    t.string "secret", null: false
    t.integer "owner_id", null: false
    t.string "owner_type"
    t.text "redirect_uri", null: false
    t.string "scopes", default: "", null: false
    t.boolean "confidential", default: true, null: false
    t.datetime "created_at", precision: nil, null: false
    t.datetime "updated_at", precision: nil, null: false
    t.index ["owner_id", "owner_type"], name: "index_oauth_applications_on_owner_id_and_owner_type"
    t.index ["uid"], name: "index_oauth_applications_on_uid", unique: true
  end

  create_table "reg_request_sections", force: :cascade do |t|
    t.integer "reg_request_id", null: false
    t.integer "section_id", null: false
    t.index ["reg_request_id"], name: "index_reg_request_sections_on_reg_request_id"
    t.index ["section_id"], name: "index_reg_request_sections_on_section_id"
  end

  create_table "reg_requests", force: :cascade do |t|
    t.integer "course_id"
    t.text "notes"
    t.datetime "created_at", precision: nil
    t.datetime "updated_at", precision: nil
    t.integer "user_id"
    t.integer "role", default: 0, null: false
  end

  create_table "registration_sections", force: :cascade do |t|
    t.integer "registration_id", null: false
    t.integer "section_id", null: false
    t.index ["registration_id"], name: "index_registration_sections_on_registration_id"
    t.index ["section_id"], name: "index_registration_sections_on_section_id"
  end

  create_table "registrations", force: :cascade do |t|
    t.integer "course_id", null: false
    t.integer "user_id", null: false
    t.datetime "created_at", precision: nil
    t.datetime "updated_at", precision: nil
    t.boolean "show_in_lists"
    t.string "tags", default: ""
    t.integer "role", default: 0, null: false
    t.datetime "dropped_date", precision: nil
    t.index ["course_id", "user_id"], name: "index_registrations_on_course_id_and_user_id", unique: true
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
    t.datetime "updated_at", precision: nil
    t.index ["review_submission_id"], name: "index_review_feedbacks_on_review_submission_id"
    t.index ["submission_id"], name: "index_review_feedbacks_on_submission_id"
  end

  create_table "sandboxes", force: :cascade do |t|
    t.string "name"
    t.integer "submission_id"
    t.datetime "created_at", precision: nil, null: false
    t.datetime "updated_at", precision: nil, null: false
  end

  create_table "sections", force: :cascade do |t|
    t.integer "course_id", null: false
    t.integer "crn", null: false
    t.string "meeting_time"
    t.integer "instructor_id", null: false
    t.integer "type", default: 0, null: false
    t.index ["course_id"], name: "index_sections_on_course_id"
    t.index ["crn", "course_id"], name: "index_sections_on_crn_and_course_id", unique: true
  end

  create_table "submission_enabled_toggles", force: :cascade do |t|
    t.integer "section_id", null: false
    t.integer "assignment_id", null: false
    t.integer "interlock_id", null: false
    t.boolean "submissions_allowed", default: false
    t.datetime "created_at", precision: nil, null: false
    t.datetime "updated_at", precision: nil, null: false
  end

  create_table "submission_views", force: :cascade do |t|
    t.integer "user_id", null: false
    t.integer "team_id"
    t.integer "assignment_id", null: false
    t.datetime "created_at", precision: nil, null: false
    t.datetime "updated_at", precision: nil, null: false
  end

  create_table "submissions", force: :cascade do |t|
    t.integer "assignment_id", null: false
    t.integer "user_id", null: false
    t.string "secret_dir"
    t.string "file_name"
    t.text "student_notes"
    t.datetime "created_at", precision: nil
    t.datetime "updated_at", precision: nil
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

  create_table "team_requests", force: :cascade do |t|
    t.integer "teamset_id", null: false
    t.integer "user_id", null: false
    t.string "partner_names", null: false
    t.datetime "created_at", precision: nil
    t.datetime "updated_at", precision: nil
    t.index ["teamset_id", "user_id"], name: "index_team_requests_on_teamset_id_and_user_id", unique: true
    t.index ["teamset_id"], name: "index_team_requests_on_teamset_id"
    t.index ["user_id"], name: "index_team_requests_on_user_id"
  end

  create_table "team_users", force: :cascade do |t|
    t.integer "team_id"
    t.integer "user_id"
    t.datetime "created_at", precision: nil, null: false
    t.datetime "updated_at", precision: nil, null: false
    t.index ["team_id", "user_id"], name: "unique_team_memebers", unique: true
  end

  create_table "teams", force: :cascade do |t|
    t.integer "course_id"
    t.date "start_date"
    t.datetime "created_at", precision: nil, null: false
    t.datetime "updated_at", precision: nil, null: false
    t.date "end_date"
    t.integer "teamset_id", null: false
  end

  create_table "teamsets", force: :cascade do |t|
    t.integer "course_id", null: false
    t.string "name"
    t.index ["course_id"], name: "index_teamsets_on_course_id"
  end

  create_table "terms", force: :cascade do |t|
    t.boolean "archived", default: false
    t.datetime "created_at", precision: nil
    t.datetime "updated_at", precision: nil
    t.integer "semester", default: 0, null: false
    t.integer "year", default: 0, null: false
    t.index ["semester", "year"], name: "index_terms_on_semester_and_year", unique: true
  end

  create_table "uploads", force: :cascade do |t|
    t.integer "user_id"
    t.string "file_name"
    t.string "secret_key"
    t.datetime "created_at", precision: nil
    t.datetime "updated_at", precision: nil
    t.integer "assignment_id", default: 0, null: false
    t.index ["assignment_id"], name: "index_uploads_on_assignment_id"
    t.index ["secret_key"], name: "index_uploads_on_secret_key", unique: true
  end

  create_table "used_subs", force: :cascade do |t|
    t.integer "user_id", null: false
    t.integer "assignment_id", null: false
    t.integer "submission_id", null: false
    t.index ["assignment_id"], name: "index_used_subs_on_assignment_id"
    t.index ["user_id", "assignment_id"], name: "unique_sub_per_user_assignment", unique: true
  end

  create_table "user_submissions", force: :cascade do |t|
    t.integer "user_id"
    t.integer "submission_id"
    t.index ["submission_id"], name: "index_user_submissions_on_submission_id"
    t.index ["user_id", "submission_id"], name: "index_user_submissions_on_user_id_and_submission_id", unique: true
    t.index ["user_id"], name: "index_user_submissions_on_user_id"
  end

  create_table "users", force: :cascade do |t|
    t.string "name", null: false
    t.string "email"
    t.boolean "site_admin"
    t.datetime "created_at", precision: nil
    t.datetime "updated_at", precision: nil
    t.string "encrypted_password", default: "", null: false
    t.string "reset_password_token"
    t.datetime "reset_password_sent_at", precision: nil
    t.datetime "remember_created_at", precision: nil
    t.integer "sign_in_count", default: 0, null: false
    t.datetime "current_sign_in_at", precision: nil
    t.datetime "last_sign_in_at", precision: nil
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

  add_foreign_key "grading_conflict_audits", "grading_conflicts"
  add_foreign_key "grading_conflict_audits", "users"
  add_foreign_key "grading_conflicts", "courses"
  add_foreign_key "grading_conflicts", "users", column: "staff_id"
  add_foreign_key "grading_conflicts", "users", column: "student_id"
  add_foreign_key "oauth_access_grants", "oauth_applications", column: "application_id"
  add_foreign_key "oauth_access_grants", "users", column: "resource_owner_id"
  add_foreign_key "oauth_access_tokens", "oauth_applications", column: "application_id"
  add_foreign_key "oauth_access_tokens", "users", column: "resource_owner_id"
  add_foreign_key "oauth_applications", "users", column: "owner_id"
end
