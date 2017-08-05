require 'test_helper'
require 'fake_upload'

class SandboxTest < ActiveSupport::TestCase
  setup do
    make_standard_course
 end

  test "run a sandbox grader lazy mode" do
    skip unless ENV['TEST_SANDBOX']

    assign = create(:assignment, type: "Files", course: @cs101, name: "Test Assignment")
    grdtar = Rails.root.join('test', 'fixtures', 'files', 'TestScript', 'test.pl')
    upload = simulated_upload(@fred, grdtar)
    upload.save!
    grdcfg = build(:grader, type: "SandboxGrader", upload: upload, params: "lazy.rb",
                   assignment: assign, order: 1)

    subprg = Rails.root.join('test', 'fixtures', 'files', 'TestScript', 'hello.c')
    sub = build(:submission, user: @john, assignment: assign, 
                upload_id: nil, ignore_late_penalty: true)
    sub.upload_file = FakeUpload.new(subprg)
    sub.save_upload
    sub.save!
    sub.autograde!

    run_background_job

    sub.reload
    assert_equal 100, sub.score
  end

  test "run a sandbox grader makefile mode" do
    skip unless ENV['TEST_SANDBOX']

    assign = create(:assignment, type: "Files", course: @cs101, name: "Test Assignment")

    grdtar = Rails.root.join('test', 'fixtures', 'files', 'HelloSingle', 'HelloSingle-grading.tar.gz')
    upload = simulated_upload(@fred, grdtar)
    upload.save!
    grdcfg = create(:grader, type: "SandboxGrader", upload: upload,
                    params: "makefile.rb", assignment: assign, order: 2)
    puts grdcfg.inspect

    subprg = Rails.root.join('test', 'fixtures', 'files', 'HelloSingle', 'hello.c')
    sub = build(:submission, user: @john, assignment: assign, 
                upload_id: nil, ignore_late_penalty: true)
    sub.upload_file = FakeUpload.new(subprg)
    sub.save_upload
    sub.save!
    sub.autograde!

    run_background_job

    sub.reload
    sub.grades.each do |g|
      puts g.inspect
      puts g.grading_output
    end
    assert_equal 100, sub.score
  end

end
