require 'test_helper'

module Api
  class GradesControllerTest < ActionController::TestCase
    setup do
      @fred = create(:user, name: 'Fred McTeacher', first_name: 'Fred', last_name: 'McTeacher', nickname: 'Fred')
      term = Term.create(semester: Term.semesters[:fall], year: Date.current.year, archived: false)
      late_per_day = LatePerDayConfig.create(days_per_assignment: 1, percent_off: 50,
                                             frequency: 1, max_penalty: 100)
      @course = Course.new(name: 'Computing 101', term: term, lateness_config: late_per_day)
      @sections = (1..3).map do |_|
        build(:section, course: @course, instructor: @fred)
      end
      @course.sections = @sections
      @course.save
      fred_reg = Registration.create(course: @course, user: @fred, role: Registration.roles[:professor],
                                     show_in_lists: false, new_sections: @sections)
      fred_reg.save_sections

      student = create(:user)
      student_reg = Registration.create(course: @course, user: student,
                                        role: Registration.roles[:student], show_in_lists: true,
                                        new_sections: [@sections.sample])
      student_reg.save_sections

      ts = Teamset.create(course: @course, name: 'Teamset 1')

      @asgn = Files.create(name: 'Assignment 2', blame: @fred, lateness_config: late_per_day,
                           course: @course, available: Time.current - 10.days, due_date: Time.current - 5.days,
                           points_available: 2.5, teamset: ts)

      junit_upload = build(:upload, user: @fred, assignment: @asgn)
      junit_upload.upload_data = FakeUpload.new(Rails.root.join('test/fixtures/files/junit-example.zip').to_s)
      @junit_grader = JunitGrader.new(assignment: @asgn, upload: junit_upload, avail_score: 50, order: 1)
      @junit_grader.test_class = 'ExampleTestClass'
      @junit_grader.errors_to_show = 3
      @junit_grader.test_timeout = 10

      @asgn.graders << @junit_grader
      @asgn.save!

      @sub = build(:submission, user: student, assignment: @asgn)
      @sub.save_upload
      @sub.save!

      @grade = Grade.new(submission: @sub, grader: @junit_grader)
      @grade.save!

      @orca_secret = 'abcdefg'
      @orca_response = {
        key: JSON.generate({ secret: @orca_secret }),
        output: 'TAP output.',
        shell_responses: [{
          stdout: 'TAP output.',
          stderr: '',
          status_code: 0,
          timed_out: false,
          cmd: ['echo', '"TAP output."']
        }],
        errors: []
      }
      @grader_dir = @grade.submission_grader_dir
      @grader_dir.mkdir
      @orca_response_base_params = { id: @grade.id }
      @junit_grader.save_orca_job_status @grade, { 'location' => 'Worker' }
      assert File.exist? @junit_grader.orca_job_status_path(@grade)
    end

    teardown do
      Upload.cleanup_test_uploads!
    end

    test 'valid orca response for submission' do
      sign_in @fred

      secret_file_path = @grader_dir.join('orca.secret')
      File.open(secret_file_path, 'w') do |f|
        f.write @orca_secret
      end

      controller_params = @orca_response_base_params.merge(@orca_response)
      post :orca_response, params: controller_params
      assert_response :success
      assert_not File.exist? secret_file_path
      assert_not File.exist? @junit_grader.orca_job_status_path(@grade)

      File.open(@grader_dir.join('result.json')) do |f|
        contents = JSON.parse(f.read)
        response_with_string_keys = JSON.parse(JSON.generate(@orca_response.except(:key)))
        assert_equal response_with_string_keys, contents
      end
    end

    test 'invalid grade id from orca response' do
      sign_in @fred

      secret_file_path = @grader_dir.join('orca.secret')
      File.open(secret_file_path, 'w') do |f|
        f.write @orca_secret
      end

      controller_params = @orca_response_base_params
                          .merge({ id: Grade.last.id + 1 })
                          .merge(@orca_response)
      post :orca_response, params: controller_params
      assert_response :bad_request
    end

    test 'missing orca secret in given grader directory' do
      sign_in @fred

      controller_params = @orca_response_base_params.merge(@orca_response)
      post :orca_response, params: controller_params
      assert_response :bad_request
    end

    test 'orca secret does not match secret file in grader dir' do
      sign_in @fred

      secret_file_path = @grader_dir.join('orca.secret')
      File.open(secret_file_path, 'w') do |f|
        f.write @orca_secret
      end

      controller_params = @orca_response_base_params
                          .merge(@orca_response)
                          .merge({ key: JSON.generate(
                            {
                              grade_id: @grade.id,
                              secret: "This is not the secret you're looking for."
                            }
                          ) })
      post :orca_response, params: controller_params
      assert_response :bad_request
    end

    test 'controller rejects otherwise valid params when key missing' do

      sign_in @fred

      controller_params = @orca_response_base_params
                          .merge(@orca_response)
                          .merge({ key: JSON.generate(
                            {
                              grade_id: @grade.id,
                              secret: "This is not the secret you're looking for."
                            }
                          ) })

      post :orca_response, params: controller_params
      assert_response :bad_request
    end

    test 'controller updates job status for job' do
      sign_in @fred

      secret_file_path = @grader_dir.join('orca.secret')
      File.open(secret_file_path, 'w') do |f|
        f.write @orca_secret
      end

      contoller_params = @orca_response_base_params
                         .merge({ key: JSON.generate({ secret: @orca_secret }) })
                         .merge({ status: { location: 'Queue', id: 10 } })
      prev_status = @junit_grader.orca_job_status_for @grade
      post :orca_response, params: contoller_params
      assert_response :success
      assert File.exist? @junit_grader.orca_job_status_path(@grade)
      assert_not prev_status == @junit_grader.orca_job_status_for(@grade)
    end
  end
end
