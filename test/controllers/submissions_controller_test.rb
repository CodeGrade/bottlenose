require 'test_helper'

class SubmissionsControllerTest < ActionController::TestCase
  setup do
    make_standard_course

    @hello = create(:assignment, course: @cs101)
    create(:assignment_grader, assignment: @hello)
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
        course_id: @cs101.id, assignment_id: @hello.id,
        submission: {
          student_notes: "@@@skip tests@@@",
          file_name: "HelloWorld.tgz",
          upload_file: upload },
      }
    end

    assert_redirected_to [@cs101, @hello, assigns(:submission)]
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

    put :update, {id: @john_hello}, { submission: { student_notes: "Bacon!",
      assignment_id: @john_hello.assignment_id, user_id: @john.id }}, {user_id: @fred.id}
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
