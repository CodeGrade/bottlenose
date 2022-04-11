require 'test_helper'

class GraderAllocationsControllerTest < ActionController::TestCase

  setup do
    make_standard_course
    @kyle = create(:user, name: "Kyle Sferrazza", first_name: "Kyle", last_name: "Sferrazza")
    @kyle_reg = create(:registration, course: @cs101, user: @kyle, 
      role: Registration::roles[:assistant])
    @jackson = create(:user, name: "Jackson Williams", first_name: "Jackson", last_name: "Williams")
    @jackson_reg = create(:registration, course: @cs101, user: @jackson,
      role: Registration::roles[:grader])
    @graders = [@kyle, @jackson]
    @assignment = create(:assignment, course: @cs101, teamset: @ts1, team_subs: false, 
      available: Date.current - 1, due_date: Date.current)
    @subs = []
    @students.each do |s|
      @subs << create(:submission, user: s, assignment: @assignment, created_at: @assignment.due_date - 2.hours)
    end
  end

  test "Given an even number of graders, a number of students (n) > the number of graders (g), equal weights, and a conflict
  between each grader and one of those students, GraderAllocations should be produced equal to the number of submissions
  (also n) where no grader is allocated a submission whose author is a student the are conflicted with." do
    
    @subs.each {|s| s.set_used_sub!}

    GradingConflict.transaction do
      @graders.each_with_index do |g, i|
        conflict = GradingConflict.create(course: @cs101, staff: g, student: @students[i])
        creation_audit = GradingConflictAudit.create(user: @fred, grading_conflict: conflict, status: conflict.status, 
          reason: "Oh god.")
        conflict.grading_conflict_audits << creation_audit
        conflict.save!
        creation_audit.save!
      end
    end

    sign_in @fred
    patch :update, params: {
      weight: {
        @fred.id => 0.0,
        @jackson.id => 1.0, 
        @kyle.id => 1.0
      }, 
      "course_id"=>@cs101.id, 
      "assignment_id"=>@assignment.id, 
      "grader_id"=>@assignment.graders.first.id
    }

    assert_redirected_to edit_course_assignment_grader_allocations_path(@cs101, @assignment, @assignment.graders.first)

    allocs = GraderAllocation.where(course: @cs101, assignment: @assignment)
    assert_equal(@subs.size, allocs.size)
    allocs.each {|a| assert_not a.conflict_currently_exists? }

  end

  test "Can get edit page." do
    sign_in @fred
    get :edit, params: {
      "course_id"=>@cs101.id, 
      "assignment_id"=>@assignment.id, 
      "grader_id"=>@assignment.graders.first.id
    }
    assert_response :success
  end

end