require 'test_helper'

class SubmissionTest < ActiveSupport::TestCase
  include ActionDispatch::TestProcess

  test "should handle fixed-late-days configs properly" do
    make_standard_course
    
    @cs101.lateness_config = FixedDaysConfig.new(max_penalty: 2, days_per_assignment: 5)
    @cs101.total_late_days = 5
    @cs101.save

    assns = (1..6).map do |i|
      create(:assignment, name: "Assignment #{i}", course: @cs101, teamset: @ts1,
             available: DateTime.current + (i - 1).weeks, due_date: DateTime.current + i.weeks,
             lateness_config: @cs101.lateness_config)
    end

    upload = fixture_file_upload('HelloWorld/HelloWorld.tgz','application/octet-stream')

    sub0 = build(:submission, user: @john, assignment: assns[0], upload_file: upload,
                 created_at: assns[0].due_date + 6.days)
    assert_not_equal false, assns[0].submission_prohibited(sub0, false) # 6 days == too late
    sub0.created_at = assns[0].due_date + 1.minute
    assert_equal false, assns[0].submission_prohibited(sub0, false) # 1 day == ok
    sub0.save_upload
    sub0.save
    sub0.set_used_everyone!


    sub1 = build(:submission, user: @john, assignment: assns[1], created_at: assns[1].due_date + 6.days)
    assert_not_equal false, assns[1].submission_prohibited(sub1, false) # 6 days == too late
    sub1.created_at = assns[1].due_date + 1.minute
    assert_equal false, assns[1].submission_prohibited(sub1, false) # 1 day == ok
    sub1.save_upload
    sub1.save
    sub1.set_used_everyone!

    
    sub2 = build(:submission, user: @john, assignment: assns[2], created_at: assns[2].due_date + 6.days)
    assert_not_equal false, assns[2].submission_prohibited(sub2, false) # 6 days == too late
    sub2.created_at = assns[2].due_date + 1.minute
    assert_not_equal false, assns[2].submission_prohibited(sub2, false) # STILL BAD -- ran out of assignments
    @cs101.lateness_config.max_penalty = 5
    @cs101.lateness_config.save
    assert_equal false, assns[2].submission_prohibited(sub2, false) # relax the permitted number of late assignments

  end

end
