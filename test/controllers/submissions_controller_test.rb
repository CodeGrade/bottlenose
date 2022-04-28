require 'test_helper'

class SubmissionsControllerTest < ActionController::TestCase
  setup do
    make_standard_course

    @hello = create(:assignment, course: @cs101, teamset: @ts1)
    @john_hello = build(:submission, user: @john, assignment: @hello)
    @john_hello.save_upload
    @john_hello.save
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
    upload = fixture_file_upload('HelloWorld/HelloWorld.tgz','application/octet-stream')

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
      upload_file = fixture_file_upload("HelloWorld/HelloWorld.#{ext}",'application/octet-stream')

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
    upload_file = fixture_file_upload("HelloSingle/hello.c",'application/octet-stream')

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

  test "a user with no enabled sections should not be able to submit" do
    @lock = Interlock.create(
      constraint: "check_section_toggles",
      assignment: @hello,
      related_assignment: @hello
    )

    upload = fixture_file_upload('HelloWorld/HelloWorld.tgz','application/octet-stream')

    sign_in @john

    post :create, params: {
      course_id: @cs101.id, 
      assignment_id: @hello.id,
      submission: {
        type: "FilesSub",
        student_notes: "@@@skip tests@@@",
        file_name: "HelloWorld.tgz",
        upload_file: upload },
    }
    prohib = assigns(:submission_prohibited)
    assert_equal prohib, "Submissions are not currently enabled for your section"
    assert_equal prohib, flash[:alert]
    assert_redirected_to [@cs101, @hello]
  end

  test "a user with one enabled section should be able to submit" do
    @lock = Interlock.create(
      constraint: "check_section_toggles",
      assignment: @hello,
      related_assignment: @hello
    )

    sets = @lock.submission_enabled_toggles.to_a
    sets.first.update_attribute(:submissions_allowed, true)

    upload = fixture_file_upload('HelloWorld/HelloWorld.tgz','application/octet-stream')

    sign_in @john

    post :create, params: {
      course_id: @cs101.id, 
      assignment_id: @hello.id,
      submission: {
        type: "FilesSub",
        student_notes: "@@@skip tests@@@",
        file_name: "HelloWorld.tgz",
        upload_file: upload },
    }
    sub = assigns(:submission).becomes(Submission)
    prohib = assigns(:submission_prohibited)
    assert_not prohib
    assert_equal flash[:notice], "Submission was successfully created."
    assert_redirected_to [@cs101, @hello, sub]
  end

  test "section toggles are applied like ormap" do
    @section2 = create(:section, course: @cs101, instructor: @fred, crn: 12346)
    mark = create(:user, name: "Mark Mischievous", first_name: "Mark", last_name: "Mischievous")
    create(:registration, course: @cs101, user: mark, role: Registration::roles[:student], show_in_lists: true, sections: [@section, @section2])
    @section2.save
    @hello2 = create(:assignment, course: @cs101, teamset: @ts1)
    @cs101.reload

    @lock = Interlock.create(
      constraint: "check_section_toggles",
      assignment: @hello2,
      related_assignment: @hello2
    )

    sets = @lock.submission_enabled_toggles.to_a

    sets.first.update_attribute(:submissions_allowed, false)
    sets.second.update_attribute(:submissions_allowed, true)
    sets.third.update_attribute(:submissions_allowed, false)

    upload = fixture_file_upload('HelloWorld/HelloWorld.tgz','application/octet-stream')

    sign_in mark

    post :create, params: {
      course_id: @cs101.id,
      assignment_id: @hello2.id,
      submission: {
        type: "FilesSub",
        student_notes: "@@@skip tests@@@",
        file_name: "HelloWorld.tgz",
        upload_file: upload
      }
    }
    sub = assigns(:submission).becomes(Submission)
    prohib = assigns(:submission_prohibited)

    assert_not prohib
    assert_equal flash[:notice], "Submission was successfully created."
    assert_redirected_to [@cs101, @hello2, sub]

    sets.second.update_attribute(:submissions_allowed, false)
    post :create, params: {
      course_id: @cs101.id,
      assignment_id: @hello2.id,
      submission: {
        type: "FilesSub",
        student_notes: "@@@skip tests@@@",
        file_name: "HelloWorld.tgz",
        upload_file: upload
      }
    }
    prohib = assigns(:submission_prohibited)
    assert_equal prohib, "Submissions are not currently enabled for any of your sections"
    assert_equal prohib, flash[:alert]
    assert_redirected_to [@cs101, @hello2]
  end

  test "can use submission for one student's grade" do
    # Basic sanity check to ensure use_for_user is working.
    # Use for User <=> ufu
    @sanity_ufu_asgn = create(:assignment, course: @cs101, teamset: @ts1, team_subs: false,
      available: Date.yesterday,
      due_date: Date.tomorrow,
      points_available: 5)

    @sanity_ufu_john_sub = create(:submission, assignment: @sanity_ufu_asgn, user: @john, created_at: Time.now - 1.day)
    
    sign_in @fred

    assert_not(UsedSub.exists?(submission: @sanity_ufu_john_sub))

    post :use_for_user, params: {
      course_id: @cs101.id,
      assignment_id: @sanity_ufu_asgn.id,
      user_id: @john.id,
      id: @sanity_ufu_john_sub.id
    }

    assert_response :redirect
    assert(UsedSub.exists?(submission: @sanity_ufu_john_sub, user: @john))
  end

  test "can use submission for all members of a team" do
    # Sanity check for use_for_everyone.
    @sanity_ev1_asgn = create(:assignment, course: @cs101, teamset: @ts1, team_subs: true,
      available: Date.yesterday,
      due_date: Date.tomorrow,
      points_available: 5)

    @sanity_ev1_js_team = Team.new(course: @cs101, teamset: @ts1, start_date: Time.now - 3.days, end_date: nil)
    @sanity_ev1_js_team.users = [@john, @sarah]
    @sanity_ev1_js_team.save

    @sanity_ev1_sarah_sub = create(:submission, assignment: @sanity_ev1_asgn, user: @sarah, team: @sanity_ev1_js_team,
                                    created_at: Time.now)
    
    assert_not(UsedSub.exists?(submission: @sanity_ev1_sarah_sub))

    sign_in @fred

    post :use_for_everyone, params: {
      course_id: @cs101.id,
      assignment_id: @sanity_ev1_asgn.id, 
      id: @sanity_ev1_sarah_sub.id
    }

    assert_response :redirect
    assert(UsedSub.exists?(submission: @sanity_ev1_sarah_sub, user: @sarah))
    assert(UsedSub.exists?(submission: @sanity_ev1_sarah_sub, user: @john))

  end
end
