require 'test_helper'

class GradeSubmissionTest < ActionDispatch::IntegrationTest
  setup do
    make_standard_course
    @tars_dir = Rails.root.join('test', 'fixtures', 'files')
  end

  teardown do
    Upload.cleanup_test_uploads!
  end

  test "Regression team lateness grade sheet" do

    # Set up: two teams, one user in both, second team has only one user. 
    # extension applied to both, one in March, one in April
    lateness_config = LatePerHourConfig.new(days_per_assignment: 1, percent_off: 2, frequency: 1, 
      max_penalty: 100)
    asn = create(:assignment, due_date: Time.new(2022, 2, 27, 12, 0), lateness_config: lateness_config,
      type: Files, team_subs: true, points_available: 5, course: @cs101, teamset: @ts1, available: Time.new(2022, 2, 20, 12, 0))
    asn.save!
    
    team_AS = Team.new(course: @cs101, start_date: Time.new(2022, 02, 01), teamset: @ts1,
      end_date: Time.new(2022, 04, 01))
    team_AS.users = [@andy, @sarah]
    team_AS.individual_extensions << IndividualExtension.new(due_date: Time.new(2022, 3, 4, 12, 0), 
                                                        created_at: Time.new(2022, 3, 3, 12, 0), assignment: asn)
    team_AS.save!

    team_S = Team.new(course: @cs101, start_date: Time.new(2022, 04, 01), end_date: nil, 
      teamset: @ts1)
    team_S.users = [@sarah]
    team_S.individual_extensions << IndividualExtension.new(due_date: Time.new(2022, 4, 3, 12, 0), 
                                                        created_at: Time.new(2022, 4, 1 , 18, 0), assignment: asn)
    team_S.save!

    sub_AS = create(:submission, assignment: asn, team: team_AS, user: @andy, 
                created_at: Time.new(2022, 3, 3, 16, 0))
    sub_AS.save!
    sub_AS.reload
    sub_AS.grades.first.update(score: 70)
    sub_AS.compute_grade!

    sub_S = create(:submission, assignment: asn, user: @sarah, team: team_S, 
                created_at: Time.new(2022, 4, 1, 20, 0))
    sub_S.save!
    sub_S.reload
    sub_S.grades.first.update(score: 90)
    sub_S.compute_grade!

    # NOTE: This ordering matters.  Specifically, it ensures
    # that Sarah's submission gets processed _before_ the paired submission,
    # which led to the bug before (we produced a hash 
    # [S => team_S, A => team_AS, S => team_AS], which overwrote Sarah's info)
    sub_S.set_used_everyone!
    sub_AS.set_used_user!(@andy)

    # Test: Lateness should be accurate in the cache
    asn.cache_effective_due_dates!([@andy, @sarah])

    lc = asn.lateness_config
    penalty = lc.late_penalty(asn, sub_S)
    assert penalty == 0
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

    assert File.exist?(pset.assignment_full_path)
    assert File.exist?(pset.grading_full_path)

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

    assert File.exist?(@submission.file_full_path)

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

    assert File.exist?(pset.assignment_full_path)
    assert File.exist?(pset.grading_full_path)

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
