require 'test_helper'

class GraphUtilsTest < ActiveSupport::TestCase

  setup do
    make_standard_course
    @kyle = create(:user, name: "Kyle Sferrazza", first_name: "Kyle", last_name: "Sferrazza")
    @kyle_reg = create(:registration, course: @cs101, user: @kyle, 
      role: Registration::roles[:assistant])
    @jackson = create(:user, name: "Jackson Williams", first_name: "Jackson", last_name: "Williams")
    @jackson_reg = create(:registration, course: @cs101, user: @jackson,
      role: Registration::roles[:grader])
    @assignment = create(:assignment, course: @cs101, teamset: @ts1, team_subs: false, 
      available: Date.current - 1, due_date: Date.current)
    @subs = []
    @students.each do |s|
      @subs << create(:submission, user: s, assignment: @assignment, created_at: @assignment.due_date - 2.hours)
    end
    @user_graders = [@jackson, @kyle]
  end

  test "Graders without conflicts should be given an equal number of submissions with equal weights." do
    weights = [[@jackson.id, 1], [@kyle.id, 1]].to_h
    # Conflicts are empty; use of variable is for testing clarity.
    conflicts = {}

    subs_for_graders = Submission.where(assignment: @assignment)
    assert_equal @subs.size, subs_for_graders.size
    
    sub_allocs = GraphUtils.assign_graders(subs_for_graders, @user_graders, weights, conflicts)
    assert sub_allocs[:unfinished].empty?
    assert_equal @subs.size, sub_allocs[:graders].values.flatten.size

  end

  test "Given graders, a conflict with a single student, equal weights, 
    and an even number of submissions, the graders should have an equal number of submissions." do
    weights = [[@jackson.id, 1], [@kyle.id, 1]].to_h
    conflicts = {@jackson.id => [@john.id]}

    subs_for_graders = Submission.where(assignment: @assignment)
    assert_equal @subs.size, subs_for_graders.size
    
    sub_allocs = GraphUtils.assign_graders(subs_for_graders, @user_graders, weights, conflicts)
    assert sub_allocs[:unfinished].empty?
    assert_equal @subs.size, sub_allocs[:graders].values.flatten.size

    john_sub = @subs.select {|sub| sub.user == @john }.first
    assert_not sub_allocs[:graders][@jackson].include?(john_sub)

  end

  test "Given graders, a conflict between one grader and sub_count - 1 students, equal weights, 
    and an even number of submissions, unfinished allocations should be non-zero" do
    weights = [[@jackson.id, 1], [@kyle.id, 1]].to_h
    conflicts = {@jackson.id => @students[0...@subs.size - 1].map(&:id)}

    subs_for_graders = Submission.where(assignment: @assignment)
    assert_equal @subs.size, subs_for_graders.size
    
    sub_allocs = GraphUtils.assign_graders(subs_for_graders, @user_graders, weights, conflicts)
    assert_not sub_allocs[:unfinished].empty?

    assert_not sub_allocs[:graders].empty?
    assert_not_equal @subs.size, sub_allocs[:graders].values.flatten.size

    conflicted_subs = @subs.select {|sub| conflicts.include?(sub.user) }
    conflicted_subs.each { |cs| assert_not sub_allocs[:graders][@jackson].include?(cs) }
  end

  test "Given graders, conflicts between each grader and all students, equal weights, 
    and an equal number of submissions, unfinished submissions and the total submissions
    should be equal" do    
    weights = [[@jackson.id, 1], [@kyle.id, 1]].to_h
    conflicts = {@jackson.id => @students.map(&:id), @kyle.id => @students.map(&:id)}

    subs_for_graders = Submission.where(assignment: @assignment)
    assert_equal @subs.size, subs_for_graders.size
    
    sub_allocs = GraphUtils.assign_graders(subs_for_graders, @user_graders, weights, conflicts)
    assert_equal @subs.size, sub_allocs[:unfinished].size
    assert sub_allocs[:graders].empty?
  end

  test "Given an even number of graders with no conflicts, an even number of assignments, and unequal weights, 
  each graders should have a different number of submissions to grade." do
  end

  test "Confirming max flow works on examples from CLRS" do
    c = {
      0 => {1 => 1000000, 2 => 1000000},
      1 => {2 => 1, 3 => 1000000},
      2 => {3=> 1000000},
      3 => {}
    }

    flow, num_discharges = GraphUtils.new.relabel_to_front(c, 0, 3)
    assert_equal c.size + 1, num_discharges
    assert_equal 2000000, flow[0].values.sum
    assert_equal 0, flow[1].values.sum
    assert_equal 0, flow[2].values.sum
    assert_equal (-2000000), flow[3].values.sum

    c_2 = {
      0 => { 1 => 16, 2 => 13 },
      1 => { 2 => 10, 3 => 12 },
      2 => { 1 => 4, 4 => 14 },
      3 => { 2 => 9, 5 => 20 },
      4 => { 3 => 7, 5 => 4 },
      5 => {},
    }

    15.times do
      flow_2, _ = GraphUtils.new.relabel_to_front(c_2, 0, 5)
      assert_equal 23, flow_2[0].values.sum
      assert_equal (-23), flow_2[5].values.sum
      (1...4).each do |n|
        assert_equal 0, flow_2[n].values.sum
      end
    end
  end

end