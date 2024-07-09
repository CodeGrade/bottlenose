require 'test_helper'

class GradesControllerTest < ActionController::TestCase
  setup do
    make_standard_course

    @hello = create(:assignment, course: @cs101, teamset: @ts1)
    @john_hello = build(:submission, user: @john, assignment: @hello)
    @john_hello.save_upload
    @john_hello.save

    @grader = @hello.graders.first
    @john_hello_grade = @john_hello.grades.first

    @orca_secret = "abcdefg"
    @orca_john_hello_response = {
      key: JSON.generate({
        grade_id: @john_hello_grade.id, # Assignment factory yields one grader.
        secret: @orca_secret
      }),
      output: "TAP output.",
      shell_responses: [{
        stdout: "TAP output.",
        stderr: "",
        status_code: 0,
        timed_out: false,
        cmd: ["echo", "\"TAP output.\""]
      }],
      errors: []
    }
    @grader_dir = @john_hello_grade.dir_for_submission(@john_hello)
    @grader_dir.mkdir
    @orca_response_base_params = {
      course_id: @cs101.id,
      assignment_id: @hello.id,
      submission_id: @john_hello.id
    }
  end

  teardown do
    Upload.cleanup_test_uploads!
  end

  test "valid orca response for submission" do
    sign_in @ken

    secret_file_path = @grader_dir.join("orca.secret")
    File.open(secret_file_path, "w") do |f|
      f.write @orca_secret
    end

    controller_params = @orca_response_base_params.merge(@orca_john_hello_response)
    post :orca_response, params: controller_params
    assert_response :success
    assert_not File.exist? secret_file_path

    File.open(@grader_dir.join("result.json")) do |f|
      contents = JSON.parse(f.read)
      response_with_string_keys = JSON.parse(JSON.generate(@orca_john_hello_response.except(:key)))
      assert_equal response_with_string_keys, contents
    end
  end

  test "invalid grade id from orca response" do
    sign_in @ken

    secret_file_path = @grader_dir.join("orca.secret")
    File.open(secret_file_path, "w") do |f|
      f.write @orca_secret
    end

    controller_params = @orca_response_base_params
                        .merge(@orca_john_hello_response)
                        .merge({ key: JSON.generate({ grade_id: Grade.last.id + 1, secret: @orca_secret }) })
    post :orca_response, params: controller_params
    assert_response :missing
  end

  test "missing orca secret in given grader directory" do
    sign_in @ken

    controller_params = @orca_response_base_params.merge(@orca_john_hello_response)
    post :orca_response, params: controller_params
    assert_response :missing
  end

  test "orca secret does not match secret file in grader dir" do
    sign_in @ken

    secret_file_path = @grader_dir.join("orca.secret")
    File.open(secret_file_path, "w") do |f|
      f.write @orca_secret
    end

    controller_params = @orca_response_base_params
                        .merge(@orca_john_hello_response)
                        .merge({ key: JSON.generate(
                                {
                                  grade_id: @john_hello_grade.id,
                                  secret: "This is not the secret you're looking for."
                                }
                              ) })
    post :orca_response, params: controller_params
    assert_response :missing
  end
end
