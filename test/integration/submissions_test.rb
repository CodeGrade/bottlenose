require 'test_helper'

class TeamsetsTest < ActionDispatch::IntegrationTest
  include Devise::Test::IntegrationHelpers
  setup do
    DatabaseCleaner.clean
    Capybara.current_driver = :webkit
    make_standard_course

    @hello = create(:assignment, course: @cs101, teamset: @ts1)
    @john_hello = create(:submission, user: @john, assignment: @hello)
  end

  teardown do
    Upload.cleanup_test_uploads!
  end

  
  def course_assn_with_teams(num_students, team_size)
    @largeCourse = create(:course)
    @largeTs = create(:teamset, course: @largeCourse)

    Registration.create(user: @fred, course: @largeCourse, new_sections: [@largeCourse.sections.first.crn],
                        role: Registration::roles[:professor], show_in_lists: true)
      .save_sections
    
    @manyUsers = (1..30).map do |n| create(:user) end
    @manyUsers.each do |u|
      r = Registration.create(user: u, course: @largeCourse, new_sections: [@largeCourse.sections.first.crn],
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
    @helloReview.save_upload && @helloReview.save

    # Visit the begin-codereview page for each user in each team
    @largeTs.teams.each do |t|
      sign_in t.users.first
      assert_difference('CodereviewMatching.count', 2) do
        get new_course_assignment_submission_path(course_id: @largeCourse.id, assignment_id: @helloReview.id)
      end
      t.users.each do |u|
        sign_in u
        assert_no_difference('CodereviewMatching.count') do
          get new_course_assignment_submission_path(course_id: @largeCourse.id, assignment_id: @helloReview.id)
        end
      end
    end
    matchings = {rev: {}, tar: {}}
    CodereviewMatching.where(assignment: @helloReview).each do |cm|
      matchings[:rev][cm.team_id] = [] unless matchings[:rev][cm.team_id]
      matchings[:rev][cm.team_id] << cm.target_team_id
      matchings[:tar][cm.target_team_id] = [] unless matchings[:tar][cm.target_team_id]
      matchings[:tar][cm.target_team_id] << cm.team_id
    end
    matchings[:tar].each do |tid, reviewers|
      assert_equal(2, reviewers.count, "Team #{tid} should receive two reviews")
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
    @helloReview.save_upload && @helloReview.save


    teams = @largeTs.teams.to_a
    CodereviewMatching.create!(assignment: @helloReview, team: teams[0], target_team: teams[2])
    CodereviewMatching.create!(assignment: @helloReview, team: teams[1], target_team: teams[3])
    CodereviewMatching.create!(assignment: @helloReview, team: teams[2], target_team: teams[4])
    
    # Visit the begin-codereview page for each user in each team
    @largeTs.teams.each do |t|
      sign_in t.users.first
      existing = CodereviewMatching.where(assignment: @helloReview, team: t).count
      assert_difference('CodereviewMatching.count', 2 - existing) do
        get new_course_assignment_submission_path(course_id: @largeCourse.id, assignment_id: @helloReview.id)
      end
      t.users.each do |u|
        sign_in u
        assert_no_difference('CodereviewMatching.count') do
          get new_course_assignment_submission_path(course_id: @largeCourse.id, assignment_id: @helloReview.id)
        end
      end
    end
    matchings = {rev: {}, tar: {}}
    CodereviewMatching.where(assignment: @helloReview).each do |cm|
      matchings[:rev][cm.team_id] = [] unless matchings[:rev][cm.team_id]
      matchings[:rev][cm.team_id] << cm.target_team_id
      matchings[:tar][cm.target_team_id] = [] unless matchings[:tar][cm.target_team_id]
      matchings[:tar][cm.target_team_id] << cm.team_id
    end
    matchings[:tar].each do |tid, reviewers|
      assert_equal(2, reviewers.count, "Team #{tid} should receive two reviews")
      assert_not_includes(reviewers, tid, "Team #{tid} should not be reviewed by itself")
    end
    matchings[:rev].each do |tid, targets|
      assert_equal(2, targets.count, "Team #{tid} should be assigned two reviews")
      assert_not_includes(targets, tid, "Team #{tid} should not review itself")
    end
  end
end
