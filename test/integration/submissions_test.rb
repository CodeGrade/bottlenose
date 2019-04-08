# coding: utf-8
require 'test_helper'

class SubmissionsTest < ActionDispatch::IntegrationTest
  include Devise::Test::IntegrationHelpers
  setup do
    DatabaseCleaner.clean
    Capybara.current_driver = :webkit
    make_standard_course
  end

  teardown do
    Upload.cleanup_test_uploads!
  end

  def clean(summary)
    summary.map do |v|
      v.delete(:used)
      [v.delete(:s).id, v]
    end.to_h
  end

  test "extensions should work with teams even when they are dissolved" do
    @as1 = create(:assignment, course: @cs101, teamset: @ts1, team_subs: true,
                  due_date: Date.current - 1.days + 12.hours,
                  points_available: 5)
    @as1.reload # needed for the lateness config
    @team1 = Team.new(course: @cs101, teamset: @ts1, start_date: Time.now - 2.days, end_date: nil)
    @team1.users = [@john, @sarah]
    @team1.save

    @sub1 = create(:submission, user: @john, assignment: @as1, team: @team1, created_at: @as1.due_date + 1.hours)
    @sub1.set_used_sub!

    assert_equal(@as1.sub_late?(@sub1), true, "Submitting late should be marked late")
    assert_equal(@as1.effective_due_date(@john, @team1), @as1.due_date,
                 "Without an extension, effective due date is assignment due date")
    assert_equal(@as1.effective_due_dates([@john], true)[@john.id], @as1.due_date,
                 "Without an extension, effective due date is assignment due date")

    @as1.cache_effective_due_dates!(@sub1.users)
    assert_equal(@as1.effective_due_date(@john, @team1), @as1.due_date,
                 "Without an extension, cached effective due date is assignment due date")
    assert_equal(@as1.effective_due_dates([@john], true)[@john.id], @as1.due_date,
                 "Without an extension, cached effective due date is assignment due date")

    @ext1 = IndividualExtension.create!(assignment: @as1, team: @team1, due_date: @as1.due_date + 1.days)
    @as1.reload
    @as1.cache_effective_due_dates!(@sub1.users)
    assert_equal(@as1.sub_late?(@sub1), false, "Submitting late, with an extension, should be marked on time")
    assert_equal(@as1.effective_due_date(@john, @team1), @ext1.due_date,
                 "With an extension, effective due date is extended due date")
    assert_equal(@as1.effective_due_dates([@john], true)[@john.id], @ext1.due_date,
                 "With an extension, effective due date is extended due date")

    @team1.dissolve(@as1.due_date + 2.hours)
    @as1.reload
    @as1.cache_effective_due_dates!(@sub1.users)
    assert_equal(@as1.sub_late?(@sub1), false, "Submitting late, with an extension, in an inactive team, should be marked on time")
    assert_equal(@as1.effective_sub_due_date(@sub1), @ext1.due_date,
                 "Submitting late, with an extension, in an inactive team, should be marked on time")

    assert_equal(@as1.effective_due_date(@john, @team1), @as1.due_date,
                 "With an extension, even with an inactive team, effective due date is normal due date")
    assert_equal(@as1.effective_due_dates([@john], true)[@john.id], @as1.due_date,
                 "With an extension, even with an inactive team, effective due date is normal due date")

    @team2 = Team.new(course: @cs101, teamset: @ts1, start_date: @team1.end_date, end_date: nil)
    @team2.users = [@john, @sarah, @andy]
    @team2.save

    @as1.reload
    @as1.cache_effective_due_dates!(@sub1.users)
    assert_equal(@as1.sub_late?(@sub1), false, "Submitting late, with an extension, in an inactive team, should be marked on time")
    assert_equal(@as1.effective_sub_due_date(@sub1), @ext1.due_date,
                 "Submitting late, with an extension, in an inactive team, should be marked on time")

    assert_equal(@as1.effective_due_date(@john, @team1), @as1.due_date,
                 "With an extension, even with an inactive team, effective due date is normal due date")
    assert_equal(@as1.effective_due_dates([@john], true)[@john.id], @as1.due_date,
                 "With an extension, even with an inactive team, effective due date is normal due date")

    @ext2 = IndividualExtension.create!(assignment: @as1, team: @team2, due_date: @as1.due_date + 3.days)
    @as1.reload
    @as1.cache_effective_due_dates!(@sub1.users)
    assert_equal(@as1.sub_late?(@sub1), false, "Submitting late, with an extension, in an inactive team, should be marked on time")
    assert_equal(@as1.effective_due_date(@john, @team2), @ext2.due_date,
                 "With a new extension on a new team for #{@john.id}, effective due date is extended due date")
    assert_equal(@as1.effective_due_dates([@john], true)[@john.id], @ext2.due_date,
                 "With a new extension on a new team for #{@john.id}, effective due date is extended due date")
  end
  
  test "course summaries should handle all submission transitions correctly" do
    @as1 = create(:assignment, course: @cs101, teamset: @ts1, due_date: Time.now - 1.days, points_available: 5)
    @as1.reload # needed for the lateness config
    @summary = clean(@cs101.score_summary(@john))
    assert_equal(@summary[@john.id],
                 {dropped: nil, min: 0.0, cur: 0.0, max: 95.0,
                  pending: 0.0, pending_names: [], unsub: 0.0, unsub_names: [],
                  remaining: 95.0},
                 "Create an assignment that's due in the past.  Before sub1 is created, max = 95, pending = 0")

    @sub1 = create(:submission, user: @john, assignment: @as1, created_at: Time.now - 2.days)
    @sub1.set_used_sub!
    @summary = clean(@cs101.score_summary(@john))
    assert (@summary[@john.id].delete(:cur).nan?)
    assert_equal(@summary[@john.id],
                 {dropped: nil, min: 0.0, max: 100.0,
                  pending: 5.0, pending_names: [@as1.name], unsub: 0.0, unsub_names: [],
                  remaining: 95.0},
                 "After creating submission but not grading it, cur == NaN, max = 100, remaining = 95")

    @sub1.create_grades!
    @sub1.grades.first.update(score: 50)
    @sub1.compute_grade!
    @summary = clean(@cs101.score_summary(@john))
    assert_equal(@summary[@john.id],
                 {dropped: nil, min: 2.5, cur: 50.0, max: 97.5,
                  pending: 0.0, pending_names: [], unsub: 0.0, unsub_names: [],
                  remaining: 95.0},
                 "After grading (at 50% quality), cur = 50%, min = 2.5, max = 97.5, remaining = 95")

    @as2 = create(:assignment, course: @cs101, teamset: @ts1, due_date: Time.now + 1.days, points_available: 5)
    @as2.reload # needed for the lateness config
    @summary = clean(@cs101.score_summary(@john))
    assert_equal(@summary[@john.id],
                 {dropped: nil, min: 2.5, cur: 25.0, max: 97.5,
                  pending: 0.0, pending_names: [], unsub: 5.0, unsub_names: [@as2.name],
                  remaining: 90.0},
                 "Create an assignment due in the future.  Cur drops to 2.5/10.0 since nothing's submitted, but max stays put")
    
    @as2.update(due_date: Time.now - 1.days)
    @summary = clean(@cs101.score_summary(@john))
    assert_equal(@summary[@john.id],
                 {dropped: nil, min: 2.5, cur: 25.0, max: 92.5,
                  pending: 0.0, pending_names: [], unsub: 0.0, unsub_names: [],
                  remaining: 90.0},
                 "After making the assignment overdue, max drops to 92.5, and unsub should now be zero")

    @sub2 = create(:submission, user: @john, assignment: @as2, created_at: Time.now - 2.days)
    @sub2.set_used_sub!
    @summary = clean(@cs101.score_summary(@john))
    assert_equal(@summary[@john.id],
                 {dropped: nil, min: 2.5, cur: 50.0, max: 97.5,
                  pending: 5.0, pending_names: [@as2.name], unsub: 0.0, unsub_names: [],
                  remaining: 90.0},
                 "After creating submission but not grading it yet, cur = 2.5/5.0, pending = 5, remaining = 90, max = min + remaining + pending")

    @sub2.create_grades!
    @sub2.grades.first.update(score: 100)
    @sub2.compute_grade!
    @summary = clean(@cs101.score_summary(@john))
    assert_equal(@summary[@john.id],
                 {dropped: nil, min: 7.5, cur: 75.0, max: 97.5,
                  pending: 0.0, pending_names: [], unsub: 0.0, unsub_names: [],
                  remaining: 90.0},
                 "After grading (at 100% quality), cur = 7.5/10, pending = 0, remaining = 90, max = min + remaining + pending")

    @as3 = create(:assignment, course: @cs101, teamset: @ts1, due_date: Time.now - 1.days, points_available: 5,
                  extra_credit: true)
    @as3.reload # needed for the lateness config
    @summary = clean(@cs101.score_summary(@john))
    assert_equal(@summary[@john.id],
                 {dropped: nil, min: 7.5, cur: 75.0, max: 97.5,
                  pending: 0.0, pending_names: [], unsub: 0.0, unsub_names: [],
                  remaining: 90.0},
                "After creating an extra credit assignment, but not submitting, nothing should change in the grades")

    @sub3 = create(:submission, user: @john, assignment: @as3, created_at: Time.now - 2.days)
    @sub3.set_used_sub!
    @summary = clean(@cs101.score_summary(@john))
    assert_equal(@summary[@john.id],
                 {dropped: nil, min: 7.5, cur: 75.0, max: 102.5,
                  pending: 5.0, pending_names: [@as3.name], unsub: 0.0, unsub_names: [],
                  remaining: 90.0},
                 "Once it's submitted, cur should stay the same, and pending should include the new sub")
    
    @sub3.create_grades!
    @sub3.grades.first.update(score: 50)
    @sub3.compute_grade!
    @summary = clean(@cs101.score_summary(@john))
    assert_equal(@summary[@john.id],
                 {dropped: nil, min: 10, cur: 100.0, max: 100,
                  pending: 0.0, pending_names: [], unsub: 0.0, unsub_names: [],
                  remaining: 90.0},
                 "After grading the extra credit, cur increases, max increases, min increases, but remaining stays the same")
  end

  test "Can handle extra credit in manual grading" do
    @as4 = create(:assignment, course: @cs101, teamset: @ts1, due_date: Time.now - 1.days, points_available: 5)
    @as4.reload # needed for the lateness config
    @regGrader = @as4.graders.first
    @regGrader.update(avail_score: 50)
    @regGrader.save
    @regGrader.reload
    @ecGrader = create(:grader, extra_credit: true, avail_score: 20)
    @as4.graders << @ecGrader
    @as4.save
    @sub4 = create(:submission, user: @john, assignment: @as4, created_at: Time.now - 2.days)
    @sub4.set_used_sub!
    @sub4.create_grades!
    @sub4.grades.find_by(grader: @regGrader).update(score: 25)
    @sub4.grades.find_by(grader: @ecGrader).update(score: 0)
    @sub4.compute_grade!
    assert_equal(100.0 * (25.0 / 50.0), @sub4.score, "Without extra credit, score is 50%")
    @sub4.grades.find_by(grader: @ecGrader).update(score: 10)
    @sub4.reload
    @sub4.compute_grade!
    assert_equal(100.0 * (25.0 + 10.0) / 50.0, @sub4.score, "With extra credit, score is boosted")
  end

  test "Can handle extra credit in exam grading" do
    @as5 = Exam.new(course: @cs101, teamset: @ts1, due_date: Time.now, points_available: 25,
                    team_subs: false, lateness_config: @cs101.lateness_config, name: "Exam",
                    available: Time.now - 1.days, blame: @fred)
    @as5.assignment_file = assign_upload_obj("Exam-EC", "exam.yaml")
    g = ExamGrader.new(avail_score: 50, order: 1)
    @as5.graders << g
    @as5.save!
    @as5.reload

    @sub5 = ExamSub.new(user: @john, assignment: @as5, created_at: @as5.due_date - 1.minute)
    @sub5.save!
    @sub5.set_used_sub!
    @sub5.create_grades!
    @as5.flattened_questions.each_with_index do |q, i|
      if q["extra"]
        @sub5.grade_question!(@fred, i, 3)
      else
        @sub5.grade_question!(@fred, i, q["weight"])
      end
    end
    g.expect_num_questions(@as5.flattened_questions.count)
    g.grade(@as5, @sub5)
    @sub5.compute_grade!
    assert_equal(100.0 * (g.normal_weight + 3.0) / g.normal_weight, @sub5.score,
                 "With extra credit, score is above 100%")
  end

  test "Can handle editing exam file after having submissions and deleting all submissions correctly" do
    @as6 = Exam.new(course: @cs101, teamset: @ts1, due_date: Time.now, points_available: 25,
                    team_subs: false, lateness_config: @cs101.lateness_config, name: "Exam1",
                    available: Time.now - 1.days, blame: @fred)
    @as6.assignment_file = assign_upload_obj("Exam", "exam.yaml")
    g = ExamGrader.new(avail_score: 50, order: 1)
    @as6.graders << g
    @as6.save!
    @as6.reload
    @sub6 = ExamSub.new(user: @john, assignment: @as6, created_at: @as6.due_date - 1.minute)
    @sub6.save!
    @sub6.set_used_sub!
    @sub6.create_grades!
    @as6.flattened_questions.each_with_index do |q, i|
      @sub6.grade_question!(@fred, i, q["weight"])
    end
    g.expect_num_questions(@as6.flattened_questions.count)
    g.grade(@as6, @sub6)
    @sub6.compute_grade!
    assert_equal(@as6.used_submissions.empty?, false,"Initially entered one grade before noticing error in YAML")
    @as6.assignment_file = assign_upload_obj("Exam", "exam-v2-correct.yaml")
    @as6.assign_attributes(:exam_disposal => "delete")
    @as6.update_submissions_if_needed
    @as6.save!
    @as6.reload
    assert_equal(@as6.used_submissions.empty?, true,"After changing exam yaml and selecting delete, submissions should be empty")

  end

  test "Can handle editing exam file after having submissions and using absolute but question count has changed" do
    @as7 = Exam.new(course: @cs101, teamset: @ts1, due_date: Time.now, points_available: 25,
                    team_subs: false, lateness_config: @cs101.lateness_config, name: "Exam1",
                    available: Time.now - 1.days, blame: @fred)
    @as7.assignment_file = assign_upload_obj("Exam", "exam.yaml")
    g = ExamGrader.new(avail_score: 50, order: 1)
    @as7.graders << g
    @as7.save!
    @as7.reload
    @sub7 = ExamSub.new(user: @john, assignment: @as7, created_at: @as7.due_date - 1.minute)
    @sub7.save!
    @sub7.set_used_sub!
    @sub7.create_grades!
    @as7.flattened_questions.each_with_index do |q, i|
      @sub7.grade_question!(@fred, i, q["weight"])
    end
    g.expect_num_questions(@as7.flattened_questions.count)
    g.grade(@as7, @sub7)
    @sub7.compute_grade!
    assert_equal(@as7.used_submissions.empty?, false,"Initially entered one grade before noticing error in YAML")
    @as7.assignment_file = assign_upload_obj("Exam", "exam-v2-error-questions.yaml")
    @as7.assign_attributes(:exam_disposal => "points")
    begin 
      @as7.update_submissions_if_needed
    rescue  
      assert_equal(@as7.errors.messages, {:base=>["Question count has changed"]}, "Checking that an error is thrown as question count has changed")
    end  
    @as7.save!
    @as7.reload
    @sub7.compute_grade!
    assert_equal(@as7.used_submissions.empty?, false,"As the question count error, submissions should be unchanged")
    assert_equal(100.0 * (g.normal_weight + 7.0) / g.normal_weight, @sub7.score.round, "Score should be unchanged")

  end

  test "Can handle editing exam file after having submissions and using absolute but weight is too low" do
    @as8 = Exam.new(course: @cs101, teamset: @ts1, due_date: Time.now, points_available: 25,
                    team_subs: false, lateness_config: @cs101.lateness_config, name: "Exam1",
                    available: Time.now - 1.days, blame: @fred)
    @as8.assignment_file = assign_upload_obj("Exam", "exam.yaml")
    g = ExamGrader.new(avail_score: 50, order: 1)
    @as8.graders << g
    @as8.save!
    @as8.reload
    @sub8 = ExamSub.new(user: @john, assignment: @as8, created_at: @as8.due_date - 1.minute)
    @sub8.save!
    @sub8.set_used_sub!
    @sub8.create_grades!
    @as8.flattened_questions.each_with_index do |q, i|
      @sub8.grade_question!(@fred, i, q["weight"])
    end
    g.expect_num_questions(@as8.flattened_questions.count)
    g.grade(@as8, @sub8)
    @sub8.compute_grade!
    assert_equal(@as8.used_submissions.empty?, false,"Initially entered one grade before noticing error in YAML")
    @as8.assignment_file = assign_upload_obj("Exam", "exam-v2-error-weight.yaml")
    @as8.assign_attributes(:exam_disposal => "points")
    begin 
      @as8.update_submissions_if_needed
    rescue  
      assert_equal(@as8.errors.messages, {:base=>["At least one grade on Question 3 is greater than the new maximum score"]}, "Checking that an error is thrown as question count has changed")
    end  
    @as8.save!
    @as8.reload
    @sub8.compute_grade!
    assert_equal(100.0 * (g.normal_weight + 7.0) / g.normal_weight, @sub8.score.round, "Score should be unchanged")
    assert_equal(@as8.used_submissions.empty?, false,"After changing exam yaml and selecting delete, submissions should be empty")

  end

  test "Can handle editing exam file after having submissions and using absolute" do
    @as9 = Exam.new(course: @cs101, teamset: @ts1, due_date: Time.now, points_available: 25,
                    team_subs: false, lateness_config: @cs101.lateness_config, name: "Exam1",
                    available: Time.now - 1.days, blame: @fred)
    @as9.assignment_file = assign_upload_obj("Exam", "exam.yaml")
    g = ExamGrader.new(avail_score: 50, order: 1)
    @as9.graders << g
    @as9.save!
    @as9.reload
    @sub9 = ExamSub.new(user: @john, assignment: @as9, created_at: @as9.due_date - 1.minute)
    @sub9.save!
    @sub9.set_used_sub!
    @sub9.create_grades!
    @as9.flattened_questions.each_with_index do |q, i|
      @sub9.grade_question!(@fred, i, q["weight"])
    end
    g.expect_num_questions(@as9.flattened_questions.count)
    g.grade(@as9, @sub9)
    @sub9.compute_grade!
    assert_equal(100.0 * (g.normal_weight + 7.0) / g.normal_weight, @sub9.score.round, "Current score with first yaml file")
    @as9.assignment_file = assign_upload_obj("Exam", "exam-v2-correct.yaml")
    @as9.assign_attributes(:exam_disposal => "points")
    @as9.update_submissions_if_needed
    @as9.save!
    @as9.reload
    @sub9.compute_grade!
    assert_equal(111.76, @sub9.score.round(2), "Corrected score with correct yaml file")

  end

  test "Can handle editing exam file after having submissions and using percentages" do
    @as10 = Exam.new(course: @cs101, teamset: @ts1, due_date: Time.now, points_available: 25,
                    team_subs: false, lateness_config: @cs101.lateness_config, name: "Exam1",
                    available: Time.now - 1.days, blame: @fred)
    @as10.assignment_file = assign_upload_obj("Exam", "exam.yaml")
    g = ExamGrader.new(avail_score: 50, order: 1)
    @as10.graders << g
    @as10.save!
    @as10.reload
    @sub10 = ExamSub.new(user: @john, assignment: @as10, created_at: @as10.due_date - 1.minute)
    @sub10.save!
    @sub10.set_used_sub!
    @sub10.create_grades!
    @as10.flattened_questions.each_with_index do |q, i|
      @sub10.grade_question!(@fred, i, q["weight"])
    end
    g.expect_num_questions(@as10.flattened_questions.count)
    g.grade(@as10, @sub10)
    @sub10.compute_grade!
    assert_equal(100.0 * (g.normal_weight + 7.0) / g.normal_weight, @sub10.score.round, "Current score with first yaml file")
    @as10.assignment_file = assign_upload_obj("Exam", "exam-v2-correct.yaml")
    @as10.assign_attributes(:exam_disposal => "percentage")
    @as10.update_submissions_if_needed
    @as10.save!
    @as10.reload
    @sub10.compute_grade!
    assert_equal(113.73, @sub10.score.round(2), "Corrected score with correct yaml file")

  end
  
  def course_assn_with_teams(num_students, team_size)
    @largeCourse = create(:course)
    @largeTs = create(:teamset, course: @largeCourse)

    Registration.create(user: @fred, course: @largeCourse, new_sections: [@largeCourse.sections.first],
                        role: Registration::roles[:professor], show_in_lists: true)
      .save_sections
    
    @manyUsers = (1..30).map do |n| create(:user) end
    @manyUsers.each do |u|
      r = Registration.create(user: u, course: @largeCourse, new_sections: [@largeCourse.sections.first],
                              role: Registration::roles[:student], show_in_lists: true)
      r.save_sections
      r
    end
    @hello = create(:assignment, course: @largeCourse, teamset: @largeTs, team_subs: true)

    sign_in @fred

    assert_difference('Team.count', 15) do
      patch randomize_course_teamset_path(course_id: @largeCourse.id, id: @largeTs.id), params: {
              random: {
                start_date: Date.today,
                end_date: Date.today + 1.week,
                teams_within: "course",
                size: 2
              } }
    end
    sign_out @fred

    @largeTs.reload
    assert_equal (num_students / team_size), @largeTs.teams.count
    # Create homeworks
    @manyUsers.each do |u|
      sign_in u

      upload_file = fixture_file_upload("files/HelloSingle/hello.c",'application/octet-stream')

      assert_difference('Submission.count') do
        post course_assignment_submissions_path(course_id: @largeCourse.id, assignment_id: @hello.id), params: {
               submission: {
                 type: "FilesSub",
                 student_notes: "@@@skip tests@@@",
                 file_name: "hello.c",
                 upload_file: upload_file },
             }
      end
      sign_out u
    end

    @hello.reload
    assert_equal(num_students, @hello.submissions.count,
                 "Everyone should have a submission")
    @manyUsers.each do |u|
      assert_equal(team_size, u.submissions_for(@hello).count,
                   "User #{u.name} should have #{team_size} submissions")
    end
    assert_equal(num_students / team_size, @hello.used_submissions.count,
                 "Each team should have #{team_size} submissions, and one each should be used")
  end
  
  test "should dynamically allocate Codereviews (team/team, no presets)" do
    course_assn_with_teams(30, 2)
    
    questions = fixture_file_upload("files/peer-eval.yaml", 'application/octet-stream')
    @helloReview = build(:assignment, course: @largeCourse, teamset: @largeTs, type: "Codereview",
                         related_assignment: @hello, team_subs: true)
    @helloReview.assignment_file = questions
    
    @helloReview.graders = [build(:grader, assignment: @helloReview, type: "CodereviewGrader",
                                  params: "peer;2;75", avail_score: 5)]
    @helloReview.save

    # Visit the begin-codereview page for each user in each team
    @largeTs.teams.each do |t|
      sign_in t.users.first
      assert_difference('CodereviewMatching.count', 2) do
        get new_course_assignment_submission_path(course_id: @largeCourse.id, assignment_id: @helloReview.id)
      end
      sign_out t.users.first
      t.users.each do |u|
        sign_in u
        assert_no_difference('CodereviewMatching.count') do
          get new_course_assignment_submission_path(course_id: @largeCourse.id, assignment_id: @helloReview.id)
        end
        sign_out u
      end
    end
    matchings = {rev: {}, tar: {}}
    CodereviewMatching.connection.reconnect!
    CodereviewMatching.where(assignment: @helloReview).each do |cm|
      matchings[:rev][cm.team_id] = [] unless matchings[:rev][cm.team_id]
      matchings[:rev][cm.team_id] << cm.target_team_id
      matchings[:tar][cm.target_team_id] = [] unless matchings[:tar][cm.target_team_id]
      matchings[:tar][cm.target_team_id] << cm.team_id
    end
    matchings[:tar].each do |tid, reviewers|
      assert_in_delta(2, reviewers.count, 1, "Team #{tid} should receive 2±1 reviews")
      assert_not_includes(reviewers, tid, "Team #{tid} should not be reviewed by itself")
    end
    matchings[:rev].each do |tid, targets|
      assert_equal(2, targets.count, "Team #{tid} should be assigned two reviews")
      assert_not_includes(targets, tid, "Team #{tid} should not review itself")
    end
  end


  test "should dynamically allocate Codereviews (team/team, some presets)" do
    course_assn_with_teams(30, 2)

    questions = fixture_file_upload("files/peer-eval.yaml", 'application/octet-stream')
    @helloReview = build(:assignment, course: @largeCourse, teamset: @largeTs, type: "Codereview",
                         related_assignment: @hello, team_subs: true)
    @helloReview.assignment_file = questions
    
    @helloReview.graders = [build(:grader, assignment: @helloReview, type: "CodereviewGrader",
                                  params: "peer;2;75", avail_score: 5)]
    @helloReview.save


    teams = @largeTs.teams.to_a
    cm1 = CodereviewMatching.create!(assignment: @helloReview, team: teams[0], target_team: teams[2])
    cm2 = CodereviewMatching.create!(assignment: @helloReview, team: teams[1], target_team: teams[3])
    cm3 = CodereviewMatching.create!(assignment: @helloReview, team: teams[2], target_team: teams[4])
    # puts cm1
    # puts cm2
    # puts cm3
    
    # Visit the begin-codereview page for each user in each team
    @largeTs.teams.each do |t|
      sign_in t.users.first
      existing = CodereviewMatching.where(assignment: @helloReview, team: t).count
      assert_difference('CodereviewMatching.count', 2 - existing) do
        get new_course_assignment_submission_path(course_id: @largeCourse.id, assignment_id: @helloReview.id)
      end
      sign_out t.users.first
      t.users.each do |u|
        sign_in u
        assert_no_difference('CodereviewMatching.count') do
          get new_course_assignment_submission_path(course_id: @largeCourse.id, assignment_id: @helloReview.id)
        end
        sign_out u
      end
    end
    matchings = {rev: {}, tar: {}}
    CodereviewMatching.connection.reconnect!
    CodereviewMatching.where(assignment: @helloReview).each do |cm|
      matchings[:rev][cm.team_id] = [] unless matchings[:rev][cm.team_id]
      matchings[:rev][cm.team_id] << cm.target_team_id
      matchings[:tar][cm.target_team_id] = [] unless matchings[:tar][cm.target_team_id]
      matchings[:tar][cm.target_team_id] << cm.team_id
    end
    matchings[:tar].each do |tid, reviewers|
      assert_in_delta(2, reviewers.count, 1, "Team #{tid} should receive 2±1 reviews, but got #{reviewers}")
      assert_not_includes(reviewers, tid, "Team #{tid} should not be reviewed by itself")
    end
    matchings[:rev].each do |tid, targets|
      assert_equal(2, targets.count, "Team #{tid} should be assigned two reviews")
      assert_not_includes(targets, tid, "Team #{tid} should not review itself")
    end
  end
end
