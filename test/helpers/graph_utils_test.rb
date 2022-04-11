require 'test_helper'

# TODO: Add assertions about the number of graders and the number of graders with allocated submissions.
class GraphUtilsTest < ActiveSupport::TestCase

  setup do
    make_standard_course
    @kyle = create(:user, name: "Kyle Sferrazza", first_name: "Kyle", last_name: "Sferrazza")
    @kyle_reg = create(:registration, course: @cs101, user: @kyle, 
      role: Registration::roles[:assistant])
    @jackson = create(:user, name: "Jackson Williams", first_name: "Jackson", last_name: "Williams")
    @jackson_reg = create(:registration, course: @cs101, user: @jackson,
      role: Registration::roles[:grader])
    @bjarne = create(:user, name: "Bjarne Stroustrop", first_name: "Bjarne", last_name: "Stroustrop")
    @bjarne_reg = create(:registration, course: @cs101, user: @bjarne,
      role: Registration::roles[:assistant])
    @james = create(:user, name: "James Gosling", first_name: "James", last_name: "Gosling")
    @james_reg = create(:registration, course: @cs101, user: @james,
      role: Registration::roles[:grader])
    @assignment = create(:assignment, course: @cs101, teamset: @ts1, team_subs: false, 
      available: Date.current - 1, due_date: Date.current)
    @subs = []
    @students.each do |s|
      @subs << create(:submission, user: s, assignment: @assignment, created_at: @assignment.due_date - 2.hours)
    end
    @all_graders = [@kyle, @jackson, @bjarne, @james]
  end

  test "Graders without conflicts should be given an equal number of submissions with equal weights." do
    weights = [[@jackson.id, 1], [@kyle.id, 1]].to_h
    # Conflicts are empty; use of variable is for testing clarity.
    conflicts = {}

    subs_for_graders = Submission.where(assignment: @assignment).includes(:users)
    assert_equal @subs.size, subs_for_graders.size
    
    sub_allocs = GraphUtils.assign_graders(subs_for_graders, @all_graders, weights, conflicts)
    assert sub_allocs[:unfinished].empty?
    assert_equal @subs.size, sub_allocs[:graders].values.flatten.size

  end

  test "Given graders, a conflict with a single student, equal weights, 
    and an even number of submissions, the graders should have an equal number of submissions." do
    weights = [[@jackson.id, 1], [@kyle.id, 1]].to_h
    conflicts = {@jackson.id => [@john.id]}

    subs_for_graders = Submission.where(assignment: @assignment).includes(:users)
    assert_equal @subs.size, subs_for_graders.size
    
    sub_allocs = GraphUtils.assign_graders(subs_for_graders, @all_graders, weights, conflicts)
    assert sub_allocs[:unfinished].empty?
    assert_equal @subs.size, sub_allocs[:graders].values.flatten.size

    john_sub = @subs.select {|sub| sub.user == @john }.first
    assert_not sub_allocs[:graders][@jackson].include?(john_sub)

  end

  test "Given graders, a conflict between one grader and sub_count - 1 students, equal weights, 
    and an even number of submissions, unfinished allocations should be non-zero" do
    weights = [[@jackson.id, 1], [@kyle.id, 1]].to_h
    conflicts = {@jackson.id => @students[0...@subs.size - 1].map(&:id)}

    subs_for_graders = Submission.where(assignment: @assignment).includes(:users)
    assert_equal @subs.size, subs_for_graders.size
    
    sub_allocs = GraphUtils.assign_graders(subs_for_graders, @all_graders, weights, conflicts)
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

    subs_for_graders = Submission.where(assignment: @assignment).includes(:users)
    assert_equal @subs.size, subs_for_graders.size
    
    sub_allocs = GraphUtils.assign_graders(subs_for_graders, @all_graders, weights, conflicts)
    assert_equal @subs.size, sub_allocs[:unfinished].size
    assert sub_allocs[:graders].empty?
  end

  test "Given an even number of graders with no conflicts, an even number of assignments, 
  and distinct weights, and a large number of submissions, each graders should have a different 
  number of submissions to grade." do
    weights = []
    (0...@all_graders.size).each do |i|
      weights << [@all_graders[i].id, (i + 1).to_f]
    end

    test_students = []
    test_regs = []
    test_subs = []
    (0...(16 * @all_graders.size)).each do |i|
      cur_student = create(:user, name: "Student #{i}", first_name: "Student", last_name: "#{i}")
      test_students << cur_student
      test_regs << create(:registration, course: @cs101, user: cur_student, 
          role: Registration::roles[:student], show_in_lists: true, new_sections: [@section.crn])
      test_subs << create(:submission, user: cur_student, assignment: @assignment, created_at: @assignment.due_date - 2.hours)
    end
    subs_for_graders = Submission.where(assignment: @assignment, user: test_students).includes(:users)

    sub_allocs = GraphUtils.assign_graders(subs_for_graders, @all_graders, weights.to_h, {})

    id_to_num_subs = {}
    sub_allocs[:graders].each {|k, v| id_to_num_subs[k.id] = v.size}
    
    assert sub_allocs[:unfinished].empty?
    assert_equal test_subs.size, sub_allocs[:graders].values.flatten.size
    assert_equal @all_graders.size, sub_allocs[:graders].values.map {|v| v.size}.to_set.size

  end

  test "Given some number of graders, MANY*g + 1 submissions, equal weights, and no conflicts, the number of submissions
  each grader is allocated should not differ by more than +/-1" do
    weights = @all_graders.map { |g| [g.id, 1.0] }.to_h
    conflicts = {}

    test_students = []
    test_regs = []
    test_subs = []
    (0...(@all_graders.size * 100 + 1)).each do |i|
      cur_student = create(:user, name: "Student #{i}", first_name: "Student", last_name: "#{i}")
      test_students << cur_student
      test_regs << create(:registration, course: @cs101, user: cur_student, 
          role: Registration::roles[:student], show_in_lists: true, new_sections: [@section.crn])
      test_subs << create(:submission, user: cur_student, assignment: @assignment, created_at: @assignment.due_date - 2.hours)
    end

    subs_for_graders = Submission.where(user: test_students, assignment: @assignment).includes(:users)
    sub_allocs = GraphUtils.assign_graders(subs_for_graders, @all_graders, weights, conflicts)

    id_to_num_subs = {}
    sub_allocs[:graders].each {|k, v| id_to_num_subs[k.id] = v.size}
    
    assert sub_allocs[:unfinished].empty?
    assert_equal test_subs.size, sub_allocs[:graders].values.flatten.size

    grader_subs = sub_allocs[:graders]
    (0...@all_graders.size).each do |i|
      if (i == @all_graders.size - 1)
        assert (grader_subs[@all_graders[i]].size - 
          grader_subs[@all_graders[0]].size).abs <= 1
      else
        assert (grader_subs[@all_graders[i]].size - 
          grader_subs[@all_graders[i+1]].size).abs <= 1
      end
    end

  end

  test "Given some number of graders, MANY*g + 1 submissions, equal weights, and each grader with 2 conflicts, the number of submissions
  each grader is allocated should not differ by more than +/-1" do
    weights = @all_graders.map { |g| [g.id, 1.0] }.to_h
    test_students = []
    test_regs = []
    test_subs = []
    (0...(@all_graders.size * 40 + 1)).each do |i|
      cur_student = create(:user, name: "Student #{i}", first_name: "Student", last_name: "#{i}")
      test_students << cur_student
      test_regs << create(:registration, course: @cs101, user: cur_student, 
          role: Registration::roles[:student], show_in_lists: true, new_sections: [@section.crn])
      test_subs << create(:submission, user: cur_student, assignment: @assignment, created_at: @assignment.due_date - 2.hours)
    end

    conflicts = {}
    s_ind = 0
    @all_graders.each do |g|
      conflicts[g.id] = []
      2.times do
        conflicts[g.id] << test_students[s_ind]
        s_ind += 1
      end
    end

    subs_for_graders = Submission.where(user: test_students, assignment: @assignment).includes(:users)
    sub_allocs = GraphUtils.assign_graders(subs_for_graders, @all_graders, weights, conflicts)

    id_to_num_subs = {}
    sub_allocs[:graders].each {|k, v| id_to_num_subs[k.id] = v.size}
    
    assert sub_allocs[:unfinished].empty?
    assert_equal test_subs.size, sub_allocs[:graders].values.flatten.size

    grader_subs = sub_allocs[:graders]
    (0...@all_graders.size).each do |i|
      if (i == @all_graders.size - 1)
        assert (grader_subs[@all_graders[i]].size - 
          grader_subs[@all_graders[0]].size).abs <= 1
      else
        assert (grader_subs[@all_graders[i]].size - 
          grader_subs[@all_graders[i+1]].size).abs <= 1
      end
    end

  end

  test "Confirming max flow works on examples from CLRS (algorithms textbook)." do
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