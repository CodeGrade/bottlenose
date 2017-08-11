require 'test_helper'
require 'fake_upload'

class SandboxTest < ActiveSupport::TestCase
  setup do
    make_standard_course
 end

  test "run a sandbox grader mode" do
    skip unless ENV['TEST_SANDBOX']
    # FIXME: This test design can't work.
    # We need to spin up a webserver for the sandbox to pull down the files.

    assign = create(:assignment, type: "Files", course: @cs101, name: "Test Assignment")

    grdtar = Rails.root.join('sandbox', 'examples', 'demo', 'demo-grading.tar.gz')
    upload = simulated_upload(@fred, grdtar)
    upload.save!
    grdcfg = create(:grader, type: "SandboxGrader", upload: upload, params: "",
                   assignment: assign, order: 1)

    subprg = Rails.root.join('sandbox', 'examples', 'demo', 'hello.tar.gz')
    sub = build(:submission, user: @john, assignment: assign, 
                upload_id: nil, ignore_late_penalty: true)
    sub.upload_file = FakeUpload.new(subprg)
    sub.save_upload
    sub.save!

    puts sub.assignment.graders.inspect

    sub.autograde!

    run_background_jobs

    sub.reload
    assert_equal 100, sub.score
  end
end
