require 'test_helper'

class SubmissionsControllerTest < ActionController::TestCase
  setup do
    make_standard_course

    @hello = create(:assignment, course: @cs101, teamset: @ts1)
    @john_hello = create(:submission, user: @john, assignment: @hello)
  end

  teardown do
    Upload.cleanup_test_uploads!
  end

  test "index should redirect" do
    sign_in @fred
    get :index, params: { assignment_id: @hello.id, course_id: @cs101.id }
    assert_response :redirect
  end

  test "should get new" do
    sign_in @john
    get :new, params: {assignment_id: @hello.id, course_id: @cs101.id }
    assert_response :success
  end

  test "should create submission" do
    upload = fixture_file_upload(
      'files/HelloWorld/HelloWorld.tgz','application/octet-stream')

    sign_in @john

    assert_difference('Submission.count') do
      post :create, params: {
        course_id: @cs101.id, 
        assignment_id: @hello.id,
        submission: {
          type: "FilesSub",
          student_notes: "@@@skip tests@@@",
          file_name: "HelloWorld.tgz",
          upload_file: upload },
      }
    end

    assert_redirected_to [@cs101, @hello.becomes(Assignment), assigns(:submission).becomes(Submission)]
  end

  test "should handle different archives" do
    ["zip", "tar", "tar.gz", "tgz"].each do |ext|
      upload_file = fixture_file_upload(
        "files/HelloWorld/HelloWorld.#{ext}",'application/octet-stream')

      sign_in @john

      assert_difference('Submission.count') do
        post :create, params: {
               course_id: @cs101.id,
               assignment_id: @hello.id,
               submission: {
                 type: "FilesSub",
                 student_notes: "@@@skip tests@@@",
                 file_name: "HelloWorld.#{ext}",
                 upload_file: upload_file },
             }
      end

      assert_redirected_to [@cs101, @hello.becomes(Assignment), assigns(:submission).becomes(Submission)]

      upload = assigns(:submission).upload
      assert(Dir.exist?(upload.upload_dir), "Upload directory exists for extension #{ext}")
      assert(Dir.exist?(upload.upload_dir.join("extracted")),
             "Upload-extraction directory exists for extension #{ext}")
      Dir.chdir(upload.upload_dir.join("extracted")) do
        assert_equal(3, Dir.glob("HelloWorld/*").count,
                     "There should be three files in the upload-extraction directory for extension #{ext}")
      end
    end
  end

  test "should handle non-archive" do
    upload_file = fixture_file_upload(
      "files/HelloSingle/hello.c",'application/octet-stream')

    sign_in @john

    assert_difference('Submission.count') do
      post :create, params: {
             course_id: @cs101.id,
             assignment_id: @hello.id,
             submission: {
               type: "FilesSub",
               student_notes: "@@@skip tests@@@",
               file_name: "hello.c",
               upload_file: upload_file },
           }
    end

    assert_redirected_to [@cs101, @hello.becomes(Assignment), assigns(:submission).becomes(Submission)]

    upload = assigns(:submission).upload
    assert(Dir.exist?(upload.upload_dir), "Upload directory exists for single file")
    assert(Dir.exist?(upload.upload_dir.join("extracted")), "Upload-extraction directory exists for single file")
    Dir.chdir(upload.upload_dir.join("extracted")) do
      assert_equal(1, Dir.glob("**").count, "There should be just the one file in the upload-extraction directory")
    end
  end

  test "should show submission" do
    sign_in @john
    get :show, params: {id: @john_hello, course_id: @cs101.id, assignment_id: @hello.id }
    assert_response :success
  end

  test "should get edit" do
    skip

    sign_in @fred
    get :edit, params: { id: @john_hello, course_id: @cs101.id, assignment_id: @hello }
    assert_response :success
  end

  test "should update submission" do
    skip

    put :update,
        {id: @john_hello},
        { submission: {
            student_notes: "Bacon!",
            type: "FilesSub",
            assignment: @john_hello.assignment,
            user_id: @john.id }
        },
        {user_id: @fred.id}
    assert_response :redirect
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

    @controller = TeamsetsController.new
    assert_difference('Team.count', 15) do
      patch :randomize, params: {
              course_id: @largeCourse.id,
              id: @largeTs.id,
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
      @controller = SubmissionsController.new
      sign_in u

      upload_file = fixture_file_upload("files/HelloSingle/hello.c",'application/octet-stream')

      assert_difference('Submission.count') do
        post :create, params: {
               course_id: @largeCourse.id,
               assignment_id: @hello.id,
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
      @controller = SubmissionsController.new
      sign_in t.users.first
      assert_difference('CodereviewMatching.count', 2) do
        get :new, params: {
              course_id: @largeCourse.id,
              assignment_id: @helloReview.id
            }
      end
      t.users.each do |u|
        @controller = SubmissionsController.new
        sign_in u
        assert_no_difference('CodereviewMatching.count') do
          get :new, params: {
                course_id: @largeCourse.id,
                assignment_id: @helloReview.id
              }
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
      @controller = SubmissionsController.new
      sign_in t.users.first
      existing = CodereviewMatching.where(assignment: @helloReview, team: t).count
      assert_difference('CodereviewMatching.count', 2 - existing) do
        get :new, params: {
              course_id: @largeCourse.id,
              assignment_id: @helloReview.id
            }
      end
      t.users.each do |u|
        @controller = SubmissionsController.new
        sign_in u
        assert_no_difference('CodereviewMatching.count') do
          get :new, params: {
                course_id: @largeCourse.id,
                assignment_id: @helloReview.id
              }
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

  
  #test "should destroy submission" do
  #  skip "Code to delete submissions intentionally disabled."
  #
  #  assert_difference('Submission.count', -1) do
  #    delete :destroy, {id: @john_hello}, {user_id: @fred.id}
  #  end
  #
  #  assert_response :redirect
  #end
end
