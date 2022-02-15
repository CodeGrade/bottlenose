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
                  available: Date.current - 1.days,
                  due_date: Date.current - 1.days + 12.hours,
                  points_available: 5)
    @as1.reload # needed for the lateness config
    @team1 = Team.new(course: @cs101, teamset: @ts1, start_date: Time.now - 2.days, end_date: nil)
    @team1.users = [@john, @sarah]
    @team1.save

    @sub1 = create(:submission, user: @john, assignment: @as1, team: @team1, created_at: @as1.due_date + 1.hours)
    @sub1.set_used_everyone!

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
    @as1 = create(:assignment, course: @cs101, teamset: @ts1,
                  available: Time.now - 2.days,
                  due_date: Time.now - 1.days, points_available: 5)
    @as1.reload # needed for the lateness config
    @summary = clean(@cs101.score_summary(@john))
    assert_equal({dropped: nil, min: 0.0, cur: 0.0, max: 95.0,
                  pending: 0.0, pending_names: [], unsub: 0.0, unsub_names: [],
                  remaining: 95.0},
                 @summary[@john.id],
                 "Create an assignment that's due in the past.  Before sub1 is created, max = 95, pending = 0")

    @sub1 = create(:submission, user: @john, assignment: @as1, created_at: Time.now - 2.days)
    @sub1.set_used_everyone!
    @summary = clean(@cs101.score_summary(@john))
    assert (@summary[@john.id].delete(:cur).nan?)
    assert_equal({dropped: nil, min: 0.0, max: 100.0,
                  pending: 5.0, pending_names: [@as1.name], unsub: 0.0, unsub_names: [],
                  remaining: 95.0},
                 @summary[@john.id],
                 "After creating submission but not grading it, cur == NaN, max = 100, remaining = 95")

    @sub1.create_grades!
    @sub1.grades.first.update(score: 50)
    @sub1.compute_grade!
    @summary = clean(@cs101.score_summary(@john))
    assert_equal({dropped: nil, min: 2.5, cur: 50.0, max: 97.5,
                  pending: 0.0, pending_names: [], unsub: 0.0, unsub_names: [],
                  remaining: 95.0},
                 @summary[@john.id],
                 "After grading (at 50% quality), cur = 50%, min = 2.5, max = 97.5, remaining = 95")

    @as2 = create(:assignment, course: @cs101, teamset: @ts1,
                  available: Time.now - 2.days,
                  due_date: Time.now + 1.days, points_available: 5)
    @as2.reload # needed for the lateness config
    @summary = clean(@cs101.score_summary(@john))
    assert_equal({dropped: nil, min: 2.5, cur: 25.0, max: 97.5,
                  pending: 0.0, pending_names: [], unsub: 5.0, unsub_names: [@as2.name],
                  remaining: 90.0},
                 @summary[@john.id],
                 "Create an assignment due in the future.  Cur drops to 2.5/10.0 since nothing's submitted, but max stays put")
    
    @as2.update(due_date: Time.now - 1.days)
    @summary = clean(@cs101.score_summary(@john))
    assert_equal({dropped: nil, min: 2.5, cur: 25.0, max: 92.5,
                  pending: 0.0, pending_names: [], unsub: 0.0, unsub_names: [],
                  remaining: 90.0},
                 @summary[@john.id],
                 "After making the assignment overdue, max drops to 92.5, and unsub should now be zero")

    @sub2 = create(:submission, user: @john, assignment: @as2, created_at: Time.now - 2.days)
    @sub2.set_used_everyone!
    @summary = clean(@cs101.score_summary(@john))
    assert_equal({dropped: nil, min: 2.5, cur: 50.0, max: 97.5,
                  pending: 5.0, pending_names: [@as2.name], unsub: 0.0, unsub_names: [],
                  remaining: 90.0},
                 @summary[@john.id],
                 "After creating submission but not grading it yet, cur = 2.5/5.0, pending = 5, remaining = 90, max = min + remaining + pending")

    @sub2.create_grades!
    @sub2.grades.first.update(score: 100)
    @sub2.compute_grade!
    @summary = clean(@cs101.score_summary(@john))
    assert_equal({dropped: nil, min: 7.5, cur: 75.0, max: 97.5,
                  pending: 0.0, pending_names: [], unsub: 0.0, unsub_names: [],
                  remaining: 90.0},
                 @summary[@john.id],
                 "After grading (at 100% quality), cur = 7.5/10, pending = 0, remaining = 90, max = min + remaining + pending")

    @as3 = create(:assignment, course: @cs101, teamset: @ts1,
                  available: Time.now - 2.days,
                  due_date: Time.now + 1.days, points_available: 5,
                  extra_credit: true)
    @as3.reload # needed for the lateness config
    @summary = clean(@cs101.score_summary(@john))
    assert_equal({dropped: nil, min: 7.5, cur: 75.0, max: 102.5,
                  pending: 0.0, pending_names: [], unsub: 5.0, unsub_names: [@as3.name],
                  remaining: 90.0},
                 @summary[@john.id],
                 "After creating an extra credit assignment due in future, but not submitting, max goes up by the available e.c.")

    @as3.update(due_date: Time.now - 1.days)
    @summary = clean(@cs101.score_summary(@john))
    assert_equal({dropped: nil, min: 7.5, cur: 75.0, max: 97.5,
                  pending: 0.0, pending_names: [], unsub: 0.0, unsub_names: [],
                  remaining: 90.0},
                 @summary[@john.id],
                 "After moving assignment to past, but not submitting, max goes down by the available e.c.")


    @sub3 = create(:submission, user: @john, assignment: @as3, created_at: Time.now - 2.days)
    @sub3.set_used_everyone!
    @summary = clean(@cs101.score_summary(@john))
    assert_equal({dropped: nil, min: 7.5, cur: 75.0, max: 102.5,
                  pending: 5.0, pending_names: [@as3.name], unsub: 0.0, unsub_names: [],
                  remaining: 90.0},
                 @summary[@john.id],
                 "Once it's submitted, cur should stay the same, and pending should include the new sub")
    
    @sub3.create_grades!
    @sub3.grades.first.update(score: 50)
    @sub3.compute_grade!
    @summary = clean(@cs101.score_summary(@john))
    assert_equal({dropped: nil, min: 10, cur: 100.0, max: 100,
                  pending: 0.0, pending_names: [], unsub: 0.0, unsub_names: [],
                  remaining: 90.0},
                 @summary[@john.id],
                 "After grading the extra credit, cur increases, max increases, min increases, but remaining stays the same")
  end

  test "Can handle extra credit in manual grading" do
    @as4 = create(:assignment, course: @cs101, teamset: @ts1,
                  available: Time.now - 2.days,
                  due_date: Time.now - 1.days, points_available: 5)
    @as4.reload # needed for the lateness config
    @regGrader = @as4.graders.first
    @regGrader.update(avail_score: 50)
    @regGrader.save
    @regGrader.reload
    @ecGrader = create(:grader, extra_credit: true, avail_score: 20)
    @as4.graders << @ecGrader
    @as4.save
    @sub4 = create(:submission, user: @john, assignment: @as4, created_at: Time.now - 2.days)
    @sub4.set_used_everyone!
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
    @sub5.set_used_everyone!
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
    @sub6.set_used_everyone!
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
    @sub7.set_used_everyone!
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
      assert_equal(@as7.errors.full_messages, ["Question count has changed"], "Checking that an error is thrown as question count has changed")
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
    @sub8.set_used_everyone!
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
      assert_equal(@as8.errors.full_messages, ["At least one grade on Question 3 is greater than the new maximum score"], "Checking that an error is thrown as question count has changed")
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
    @sub9.set_used_everyone!
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
    @sub10.set_used_everyone!
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

  test "Can handle changing exam file with a incorrect format one" do
    @as11 = Exam.new(course: @cs101, teamset: @ts1, due_date: Time.now, points_available: 25,
                    team_subs: false, lateness_config: @cs101.lateness_config, name: "Exam",
                    available: Time.now - 1.days, blame: @fred)
    @as11.assignment_file = assign_upload_obj("Exam", "exam-incorrect-format.yaml")
    g = ExamGrader.new(avail_score: 50, order: 1)
    @as11.graders << g
    begin
      @as11.save!
    rescue  
      assert_equal(["Could not parse the supplied file"], @as11.errors.full_messages, "Checking that an error if the exam file is malformed") 
    end
    @as11.assignment_file = assign_upload_obj("Exam-EC", "exam.yaml")
    @as11.save!
    @as11.reload
    @as11.assignment_file = assign_upload_obj("Exam", "exam-incorrect-format.yaml")
    @as11.save!
    assert_equal(["Could not parse the supplied file"], @as11.errors.full_messages, "Checking that an error if the exam file is malformed after update")
 
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
                         related_assignment: @hello, team_subs: true, blame: @fred)
    @helloReview.assignment_file = questions
    
    @helloReview.graders = [build(:grader, assignment: @helloReview, type: "CodereviewGrader",
                                  params: "peer;2;75", avail_score: 5)]
    @helloReview.save!

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
                         related_assignment: @hello, team_subs: true, blame: @fred)
    @helloReview.assignment_file = questions
    
    @helloReview.graders = [build(:grader, assignment: @helloReview, type: "CodereviewGrader",
                                  params: "peer;2;75", avail_score: 5)]
    @helloReview.save!


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

  test "should be able to use a different individiual submission for user with use_for_user" do
    # Abbreviation for this => iaufu
    # This notation is used for objects referencing it.
    @ind_asgn_use_for_user = create(:assignment, course: @cs101, teamset: @ts1, team_subs: false,
      available: Date.current,
      due_date: Date.current + 1.day,
      points_available: 5)
    @iaufu_grader = create(:grader, assignment: @ind_asgn_use_for_user)

    @john_ind_sub_1 = create(:submission, user: @john, assignment: @ind_asgn_use_for_user, 
                                created_at: @ind_asgn_use_for_user.available + 1.hours)
    @john_ind_sub_2 = create(:submission, user: @john, assignment: @ind_asgn_use_for_user, 
                                created_at: @ind_asgn_use_for_user.available + 2.hours)

    # UsedSub for John on this assignment should not exist yet.
    assert_not(UsedSub.exists?(assignment_id: @ind_asgn_use_for_user.id, user_id: @john.id, 
                                submission_id: @john_ind_sub_1.id));
    assert_not(UsedSub.exists?(assignment_id: @ind_asgn_use_for_user.id, user_id: @john.id, 
                                submission_id: @john_ind_sub_2.id));
    
    # Set Sub2 Used
    @john_ind_sub_2.set_used_user!(@john)

    # Assert existence of UsedSub for John with sub2 on this assignment. Sub1 should
    # not have a used sub.
    assert_not(UsedSub.exists?(assignment_id: @ind_asgn_use_for_user.id, user_id: @john.id, 
      submission_id: @john_ind_sub_1.id));
    assert(UsedSub.exists?(assignment_id: @ind_asgn_use_for_user.id, user_id: @john.id, 
      submission_id: @john_ind_sub_2.id));

    # Create GraderAlloc for the submission.
    @iaufu_alloc = create(:grader_allocation, course: @cs101, assignment: @ind_asgn_use_for_user, 
                            submission: @john_ind_sub_2, who_grades_id: @fred.id)
    
    # Assert GraderAlloc information is still the same.
    assert_equal(@cs101, @iaufu_alloc.course)
    assert_equal(@ind_asgn_use_for_user.id, @iaufu_alloc.assignment_id)
    assert_equal(@fred, @iaufu_alloc.who_grades)
    assert_equal(@john_ind_sub_2, @iaufu_alloc.submission)

    # Should NOT be submission 1.
    assert_not_equal(@iaufu_alloc.submission, @john_ind_sub_1)

    # Set Sub1 Used
    @john_ind_sub_1.set_used_user!(@john)
    @iaufu_alloc.reload

    # Should be a UsedSub for John on this assignment for sub1, and not for sub2.
    assert(UsedSub.exists?(assignment_id: @ind_asgn_use_for_user.id, user_id: @john.id, 
      submission_id: @john_ind_sub_1.id));
    assert_not(UsedSub.exists?(assignment_id: @ind_asgn_use_for_user.id, user_id: @john.id, 
      submission_id: @john_ind_sub_2.id));

    # All alloc data Except :submission should not have changed.
    assert_equal(@cs101, @iaufu_alloc.course)
    assert_equal(@ind_asgn_use_for_user.id, @iaufu_alloc.assignment_id)
    assert_equal(@fred, @iaufu_alloc.who_grades)

    # Sub1 should be in grader allocation; sub2 should not.
    assert_equal(@john_ind_sub_1.id, @iaufu_alloc.submission_id);
    assert_not_equal(@john_ind_sub_2.id, @iaufu_alloc.submission_id);
    
  end

  test "should be able to use a different submission for entire team (use_for_everyone)" do
    @team_ev1_asgn = create(:assignment, course: @cs101, teamset: @ts1, team_subs: true,
      available: Date.current,
      due_date: Date.current + 1.day,
      points_available: 5)
    
    @js_team_ev1 = Team.new(course: @cs101, teamset: @ts1, start_date: Time.now - 2.days, end_date: nil)
    @js_team_ev1.users = [@john, @sarah]
    @js_team_ev1.save

    @john_ev1_sub = create(:submission, user: @john, team: @js_team_ev1, assignment: @team_ev1_asgn, 
        created_at: @team_ev1_asgn.available + 1.hours)
    @sarah_ev1_sub = create(:submission, user: @sarah, team: @js_team_ev1, assignment: @team_ev1_asgn, 
      created_at: @team_ev1_asgn.available + 2.hours)

    assert_not(UsedSub.exists?(submission_id: @john_ev1_sub.id))
    assert_not(UsedSub.exists?(submission_id: @sarah_ev1_sub.id))

    # Sarah's sub is the used submission.
    @sarah_ev1_sub.set_used_everyone!

    assert_not(UsedSub.exists?(submission_id: @john_ev1_sub.id))
    assert(UsedSub.exists?(submission_id: @sarah_ev1_sub.id, user_id: @john.id))
    assert(UsedSub.exists?(submission_id: @sarah_ev1_sub.id, user_id: @sarah.id))

    # Create a GraderAllocation for the UsedSubmission.
    @ev1_sub_ga = create(:grader_allocation, course: @cs101, assignment: @team_ev1_asgn, 
                          submission: @sarah_ev1_sub, who_grades_id: @fred.id)
    
    # John's submission is set for everyone's use.
    @john_ev1_sub.set_used_everyone!
    @ev1_sub_ga.reload

    assert(UsedSub.exists?(submission_id: @john_ev1_sub.id, user_id: @john.id))
    assert(UsedSub.exists?(submission_id: @john_ev1_sub.id, user_id: @sarah.id))
    assert_not(UsedSub.exists?(submission_id: @sarah_ev1_sub.id))

    # Grader allocation info should be same, EXCEPT for the submission.
    assert_equal(@cs101.id, @ev1_sub_ga.course_id)
    assert_equal(@team_ev1_asgn.id, @ev1_sub_ga.assignment_id)
    assert_equal(@fred.id, @ev1_sub_ga.who_grades_id)
    assert_equal(@john_ev1_sub.id, @ev1_sub_ga.submission_id)
    assert_not_equal(@sarah_ev1_sub.id, @ev1_sub_ga.submission_id)

  end

  test "should allow a member of a current team to use an old submission with a dissolved prior team for grading" do
    @asgn_team_diss = create(:assignment, course: @cs101, teamset: @ts1, team_subs: true,
      available: Date.yesterday,
      due_date: Date.tomorrow,
      points_available: 5)
    
    @js_team_to_diss = Team.new(course: @cs101, teamset: @ts1, start_date: Time.now - 3.days, end_date: nil)
    @js_team_to_diss.users = [@john, @sarah]
    @js_team_to_diss.save

    @js_john_sub_diss = create(:submission, assignment: @asgn_team_diss, user: @john, team: @js_team_to_diss,
                            created_at: Time.now - 1.day)
    
    @js_john_sub_diss.set_used_everyone!

    # Vaildate UsedSub data (one for j and s)
    assert(UsedSub.exists?(assignment: @asgn_team_diss, submission: @js_john_sub_diss, user: @john))
    assert(UsedSub.exists?(assignment: @asgn_team_diss, submission: @js_john_sub_diss, user: @sarah))

    @js_john_sub_diss_ga = create(:grader_allocation, assignment: @asgn_team_diss, 
                                    course: @cs101, submission: @js_john_sub_diss, who_grades_id: @fred.id)

    # Dissolve the team.
    @js_team_to_diss.dissolve(Date.today)
    # @js_john_sub_diss_ga.reload

    assert_not(UsedSub.exists?(assignment: @asgn_team_diss, submission: @js_john_sub_diss, user: @john))
    assert_not(UsedSub.exists?(assignment: @asgn_team_diss, submission: @js_john_sub_diss, user: @sarah))

    assert_not(GraderAllocation.exists?(submission: @js_john_sub_diss))
    # assert_equal(@fred.id, @js_john_sub_diss_ga.who_grades_id)
    # assert_equal(@asgn_team_diss.id, @js_john_sub_diss_ga.assignment_id)
    # assert_equal(@cs101.id, @js_john_sub_diss_ga.course_id)
    # assert_equal(@js_john_sub_diss.id, @js_john_sub_diss_ga.submission_id)

    # Create new team and submission for them.
    @jc_team_no_diss = Team.new(course: @cs101, teamset: @ts1, start_date: Time.now, end_date: nil)
    @jc_team_no_diss.users = [@john, @claire]
    @jc_team_no_diss.save

    @jc_john_sub_no_diss = create(:submission, assignment: @asgn_team_diss, user: @john, 
                              team: @jc_team_no_diss, created_at: Time.now)
    
    # Set the submission to be used for everyone.
    @jc_john_sub_no_diss.set_used_everyone!

    # UsedSub for John should now be jc_..., as well as for Claire. Sarah should still use old one.
    assert_not(UsedSub.exists?(assignment: @asgn_team_diss, submission: @js_john_sub_diss, user: @john))
    # assert(UsedSub.exists?(assignment: @asgn_team_diss, submission: @js_john_sub_diss, user: @sarah))

    assert(UsedSub.exists?(assignment: @asgn_team_diss, submission: @jc_john_sub_no_diss, user: @john))
    assert(UsedSub.exists?(assignment: @asgn_team_diss, submission: @jc_john_sub_no_diss, user: @claire))
    assert_not(UsedSub.exists?(assignment: @asgn_team_diss,  user: @sarah))


    # Set John's previous submission with Sarah to be used for John. 
    @js_john_sub_diss.set_used_user!(@john)

    # Claire's UsedSub should be the same, John's should not.
    assert_not(UsedSub.exists?(assignment: @asgn_team_diss, submission: @jc_john_sub_no_diss, user: @john))
    assert(UsedSub.exists?(assignment: @asgn_team_diss, submission: @js_john_sub_diss, user: @john))
    assert(UsedSub.exists?(assignment: @asgn_team_diss, submission: @jc_john_sub_no_diss, user: @claire))
    assert_not(UsedSub.exists?(assignment: @asgn_team_diss,  user: @sarah))


    
  end

  test "should allow two users in the same team to use two different team submissions for grading" do
    # ufu is Use for User
    @team_asgn_2_ufu = create(:assignment, course: @cs101, teamset: @ts1, team_subs: true,
      available: Date.current,
      due_date: Date.current + 1.day,
      points_available: 5)
    
    @js_team_ufu = Team.new(course: @cs101, teamset: @ts1, start_date: Time.now - 2.days, end_date: nil)
    @js_team_ufu.users = [@john, @sarah]
    @js_team_ufu.save

    @john_ufu_sub = create(:submission, team: @js_team_ufu, assignment: @team_asgn_2_ufu, 
        created_at: @team_asgn_2_ufu.available + 1.hours)
    @sarah_ufu_sub = create(:submission, team: @js_team_ufu, assignment: @team_asgn_2_ufu, 
      created_at: @team_asgn_2_ufu.available + 2.hours)

    assert_not(UsedSub.exists?(submission_id: @john_ufu_sub.id))
    assert_not(UsedSub.exists?(submission_id: @sarah_ufu_sub.id))

    # Sarah's sub is the used submission for everyone.
    @sarah_ufu_sub.set_used_everyone!

    assert(UsedSub.exists?(submission_id: @sarah_ufu_sub.id, user_id: @sarah.id))
    assert(UsedSub.exists?(submission_id: @sarah_ufu_sub.id, user_id: @john.id))
    assert_not(UsedSub.exists?(submission_id: @john_ufu_sub.id))

    # Create a GraderAllocation for the UsedSubmission.
    @ufu_sub_ga = create(:grader_allocation, course: @cs101, assignment: @team_asgn_2_ufu, 
                          submission: @sarah_ufu_sub, who_grades_id: @fred.id)
    
    # John's submission is set for him Only. Sarah's UsedSub data and associated GraderAlloc 
    # data should not change.
    @john_ufu_sub.set_used_user!(@john)
    @ufu_sub_ga.reload

    assert(UsedSub.exists?(submission_id: @john_ufu_sub.id, user_id: @john.id))
    assert_not(UsedSub.exists?(submission_id: @john_ufu_sub.id, user_id: @sarah.id))
    assert(UsedSub.exists?(submission_id: @sarah_ufu_sub.id, user_id: @sarah.id))
    assert_not(UsedSub.exists?(submission_id: @sarah_ufu_sub.id, user_id: @john.id))

    # Grader allocation info should not change.
    assert_equal(@cs101.id, @ufu_sub_ga.course_id)
    assert_equal(@team_asgn_2_ufu.id, @ufu_sub_ga.assignment_id)
    assert_equal(@fred.id, @ufu_sub_ga.who_grades_id)
    assert_equal(@sarah_ufu_sub.id, @ufu_sub_ga.submission_id)
    assert_not(GraderAllocation.exists?(submission_id: @john_ufu_sub.id))

    @john_ufu_sub.set_used_user!(@sarah)
    @ufu_sub_ga.reload

    assert(UsedSub.exists?(submission_id: @john_ufu_sub.id, user_id: @john.id))
    assert(UsedSub.exists?(submission_id: @john_ufu_sub.id, user_id: @sarah.id))
    assert_not(UsedSub.exists?(submission_id: @sarah_ufu_sub.id))

    # Grader allocation info should stay the same EXCEPT we now use John's sub.
    assert_equal(@cs101.id, @ufu_sub_ga.course_id)
    assert_equal(@team_asgn_2_ufu.id, @ufu_sub_ga.assignment_id)
    assert_equal(@fred.id, @ufu_sub_ga.who_grades_id)
    assert_equal(@john_ufu_sub.id, @ufu_sub_ga.submission_id)
    assert_not(GraderAllocation.exists?(submission_id: @sarah_ufu_sub.id))
    
  end


end
