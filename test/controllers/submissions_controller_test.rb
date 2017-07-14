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
