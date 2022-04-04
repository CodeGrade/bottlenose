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
    assert_equal @subs.size, sub_allocs[:graders].size

  end

  test "Given graders, a conflict with a single student, equal weights, 
    and an even number of submissions, the graders should have an equal number of submissions." do
    weights = [[@jackson.id, 1], [@kyle.id, 1]].to_h
    conflicts = {@jackson.id => [@john]}

    subs_for_graders = Submission.where(assignment: @assignment)
    assert_equal @subs.size, subs_for_graders.size
    
    sub_allocs = GraphUtils.assign_graders(subs_for_graders, @user_graders, weights, conflicts)
    assert sub_allocs[:unfinished].empty?
    assert_equal @subs.size, sub_allocs[:graders].size

    john_sub = @subs.select {|sub| sub.user == @john }.first
    assert_not sub_allocs[@jackson.id].includes?(john_sub)

  end

  test "Given graders, a conflict between one grader and sub_count - 1 students, equal weights, 
    and an even number of submissions, unfinished allocations should be non-zero" do
    weights = [[@jackson.id, 1], [@kyle.id, 1]].to_h
    conflicts = {@jackson.id => @students[0..@subs.size - 1]}

    subs_for_graders = Submission.where(assignment: @assignment)
    assert_equal @subs.size, subs_for_graders.size
    
    sub_allocs = GraphUtils.assign_graders(subs_for_graders, @user_graders, weights, conflicts)
    assert_not sub_allocs[:unfinished].empty?
    assert_difference @subs.size, sub_allocs[:graders].size

    conflicted_subs = @subs.select {|sub| conflicts.include?(sub.user) }
    conflicted_subs.each { |cs| assert_not sub_allocs[:graders][@jackson.id].incude?(cs) }
  end

  test "Given graders, conflicts between each grader and all students, equal weights, 
    and an equal number of submissions, unfinished submissions and the total number of submissions
    should be equal."
    
    # weights = [[@jackson.id, 1], [@kyle.id, 1]].to_h
    # conflicts = {@jackson.id => @students, @kyle.id => @students}

    # subs_for_graders = Submission.where(assignment: @assignment)
    # assert_equal @subs.size, subs_for_graders.size
    
    # sub_allocs = GraphUtils.assign_graders(subs_for_graders, @user_graders, weights, conflicts)
    # assert_equal @subs.size, sub_allocs[:unfinished].size
    # assert sub_allocs[:graders].empty?
end