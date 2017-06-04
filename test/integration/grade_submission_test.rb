require 'test_helper'

class GradeSubmissionTest < ActionDispatch::IntegrationTest
  setup do
    make_standard_course
    @tars_dir = Rails.root.join('test', 'fixtures', 'files')
  end

  teardown do
    Upload.cleanup_test_uploads!
  end

  test "teacher sets ignore late penalty flag" do
    skip

    pset = make_assignment(@bucket, "HelloWorld")
    sub  = make_submission(@john, pset, "john.tar.gz")

    visit "http://test.host/main/auth?email=#{@fred.email}&key=#{@fred.auth_key}"
    visit 'http://test.host/' + edit_submission_path(sub)

    check 'submission[ignore_late_penalty]'
    click_button 'Set Teacher Score'

    assert has_content?('View Submission')

    assert sub.reload.ignore_late_penalty?, "Ignore late penalty is set."
  end

  test "teacher manually submit a grade" do
    skip

    pset = create(:assignment, bucket: @bucket, course: @cs101)

    score0 = @john_reg.total_score

    visit "http://test.host/main/auth?email=#{@fred.email}&key=#{@fred.auth_key}"
    click_link 'Your Courses'
    click_link @cs101.name
    click_link pset.name
    click_link 'Manually Add Student Grade'

    select @john.name,  :from => 'submission[user_id]'
    fill_in 'submission[teacher_notes]', :with => 'manually entered grade'
    fill_in 'submission[teacher_score]', :with => '85'
    click_button 'Save Grade'

    sub = Submission.find_by_teacher_notes('manually entered grade')
    assert_equal sub.user_id, @john.id
    assert_equal 85, sub.score

    # Make sure score summary updates properly.
    assert_not_equal(@john_reg.reload.total_score, score0, "Updated summary")
  end

  test "submit and grade a submission" do
    skip

    pset = make_assignment(@bucket, 'HelloWorld')

    assert File.exists?(pset.assignment_full_path)
    assert File.exists?(pset.grading_full_path)

    # Log in as a student.
    visit "http://test.host/main/auth?email=#{@john.email}&key=#{@john.auth_key}"

    click_link 'Your Courses'
    click_link @cs101.name

    click_link pset.name

    click_link 'New Submission'

    fill_in 'Student notes', :with => "grade_submission_test"
    attach_file 'Upload', assign_upload('HelloWorld', 'john.tar.gz')
    click_button 'Create Submission'

    repeat_until(60) do
      sleep 2
      @submission = Submission.find_by_student_notes("grade_submission_test")
      not @submission.auto_score.nil?
    end

    assert_equal 100, @submission.auto_score

    assert File.exists?(@submission.file_full_path)

    # Download the submissions tarball.
    visit "http://test.host/main/auth?email=#{@fred.email}&key=#{@fred.auth_key}"

    click_link 'Your Courses'
    click_link @cs101.name
    click_link pset.name
    click_link 'Tarball of Submissions'

    assert_equal "application/x-gzip", page.response_headers["Content-Type"]
  end

  test "grade an assignment with no submission" do
    skip

    # Add test assignment.
    visit "http://test.host/main/auth?email=#{@fred.email}&key=#{@fred.auth_key}"

    click_link 'Your Courses'
    click_link @cs101.name
    click_link 'New Assignment'

    fill_in 'Name', :with => "An Assignment With No Submission"
    click_button 'Create Assignment'

    within("#u#{@john.id}_new_submission") do
      fill_in("submission[teacher_score]", with: '81')
      click_button 'Create Submission'
    end

    assert has_content?('81')

    # TODO: Add javascript testing so we can actually test the remote: true
    # submission.
  end

  test "submit and grade a single file submission with specially valued tests" do
    skip

    pset = create(:assignment, bucket: @bucket, course: @bucket.course, name: "HelloSingle")

    # Add test assignment.
    visit "http://test.host/main/auth?email=#{@fred.email}&key=#{@fred.auth_key}"

    click_link 'Your Courses'
    click_link @cs101.name

    click_link pset.name
    click_link 'Edit this Assignment'

    assign_file = @tars_dir.join('HelloSingle', 'hello.c')
    attach_file 'Assignment file', assign_file
    grading_file = @tars_dir.join('HelloSingle', 'HelloSingle-grading.tar.gz')
    attach_file 'Grading file', grading_file
    click_button 'Update Assignment'

    pset.reload

    assert File.exists?(pset.assignment_full_path)
    assert File.exists?(pset.grading_full_path)

    # Log in as a student.
    visit "http://test.host/main/auth?email=#{@john.email}&key=#{@john.auth_key}"

    click_link 'Your Courses'
    click_link @cs101.name
    click_link pset.name
    click_link 'New Submission'

    fill_in 'Student notes', :with => "grade_submission_test"
    attach_file 'Upload', assign_file
    click_button 'Create Submission'

    repeat_until(60) do
      sleep 2
      @submission = Submission.find_by_student_notes("grade_submission_test")
      not @submission.auto_score.nil?
    end

    assert_equal 75, @submission.auto_score
  end

  private

  def repeat_until(timeout)
    t0 = Time.now
    result = false
    until result or Time.now > t0 + timeout
      result = yield
    end
  end
end
