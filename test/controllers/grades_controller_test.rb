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
      }]
    }
    @grader_dir = @john_hello.upload.grader_path(@grader)
    @grader_dir.mkdir
    @orca_response_base_params = {
      course_id: @cs101.id,
      assignment_id: @hello.id,
      submission_id: @john_hello.id,
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
    
    log_file_path, output_file_path = @grader_dir.join("orca_logs.json"), @grader_dir.join("orca_output")
    assert File.exist? log_file_path
    File.open(log_file_path) do |f|
      expected_logs = JSON.generate(
        {shell_responses: @orca_john_hello_response[:shell_responses],
          execution_errors: nil
          }
        )
      logs = f.read
      assert_equal(expected_logs, logs)
    end
    assert File.exist? output_file_path
    File.open(output_file_path) do |f|
      expected_output = @orca_john_hello_response[:output]
      output = f.read
      assert_equal(expected_output, output)
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
                          .merge({key: JSON.generate({grade_id: 12345, secret: @orca_secret})})
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
                          .merge({key: JSON.generate({grade_id: @john_hello_grade.id, 
                            secret: "This is not the secret you're looking for."})})
    post :orca_response, params: controller_params
    assert_response :missing
  end
end