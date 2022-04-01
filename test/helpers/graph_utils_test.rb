require 'test_helper'

class GraphUtilsTest < ActiveSupport::TestCase

  setup do
    make_standard_course
    @kyle = create(:user, name: "Kyle Sferrazza", first_name: "Kyle", last_name: "Sferrazza")
    @kyle_reg = create(:registration, course: @cs101, user: @kyle, 
      role: Registration::roles[:assistant], show_in_lists: false)
    @jackson = create(:user, name: "Jackson Williams", first_name: "Jackson", last_name: "Williams")
    @jackson_reg = create(:registration, course: @cs101, user: @jackson,
      role: Registration::roles[:grader])
    @assignment = create(:assignment, course: @cs101, teamset: @ts1, team_subs: false, 
      available: Date.current - 1, due_date: Date.current)
    @subs = []
    @students.each do |s|
      @subs << create(:submission, user: s, assignment: @assignment, created_at: @asssignment.due_date - 2.hours)
    end
    @user_graders = [@jackson, @kyle]
  end

  test "Graders without conflicts should be allocated to based on their weights" do
    weights = [[@jackson, 1], [@kyle, 1]].to_h
    # Conflicts are empty; use of variable is for testing clarity.
    conflicts = {}

    assigned_subs, unfinished = GraphUtils.assign_graders(@subs, @user_graders, weights, conflicts)
    assert_equal 0, unfinished
    assert_equal @subs.size, assigned_subs.size

  end

  test "A grader will not be allocated to the submission of a student they have a conflict with." do
  end


end