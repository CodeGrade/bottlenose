require 'test_helper'

class GradingConflictsControllerTest < ActionController::TestCase
  
  setup do
    make_standard_course
    @jackson = create(:user, name: "Jackson Williams", first_name: "Jackson", last_name: "Williams")
    @jackson_reg = create(:registration, course: @cs101, user: @jackson,
      role: Registration::roles[:grader])
    @kyle = create(:user, name: "Kyle Sferrazza", first_name: "Kyle", last_name: "Sferrazza")
    @kyle_reg = create(:registration, course: @cs101, user: @kyle, 
      role: Registration::roles[:assistant])
    GradingConflict.transaction do
      @inactive_conflict = GradingConflict.create(course: @cs101, staff: @kyle, student: @students.first,
        status: :inactive)
      creation_audit = GradingConflictAudit.create(user: @fred, grading_conflict: @inactive_conflict, 
        status: @inactive_conflict.status, reason: "Why would Hank do it this way?")
      @inactive_conflict.grading_conflict_audits << creation_audit
      @inactive_conflict.save!
      creation_audit.save!
    end
  end

  test "Should get index." do
    sign_in @fred
  end

  test "Should get new." do
    sign_in @fred
    get :new, params: {
      course_id: @cs101.id
    }
    assert_response :success
  end

  test "Should get show." do
    sign_in @fred
    get :show, params: {
      course_id: @cs101.id,
      id: @inactive_conflict.id
    }
    assert_response :success
  end

  test "Can create and destroy a conflict." do
    sign_in @jackson
    assert_difference('GradingConflict.count') do 
      post :create, params: {
        course_id: @cs101.id,
        grading_conflict: {
          student_id: @students.first.id,
          staff_id: @jackson.id,
          reason: "We do not get along."
        }
      }
    end
    assert_response :redirect
    conflict = GradingConflict.find_by(course: @cs101, staff: @jackson, student: @students.first)

    delete :destroy, params: {
      course_id: @cs101.id,
      id: conflict.id
    }
    assert_response :redirect
    assert_nil GradingConflict.find_by(course: @cs101, staff: @jackson, student: @students.first)
    assert_nil flash[:alert]
  end

  test "Can update a conflict." do
    sign_in @fred 
    patch :update, params: {
      course_id: @cs101.id,
      id: @inactive_conflict.id,
      grading_conflict: {
        status: :inactive,
        reason: "This is a problem still."
      }
    }
    assert_response :redirect
    assert_not_nil flash[:notice]

    puts @inactive_conflict.status
  end

  test "Can resubmit a conflict request." do
    sign_in @students.first 
    patch :resubmit_conflict_request, params: {
      course_id: @cs101.id,
      id: @inactive_conflict.id,
      grading_conflict: {
        status: :pending,
        reason: "This is a problem still."
      }
    }
    assert_response :redirect
    assert_not_nil flash[:notice]
    puts @inactive_conflict.status
  end

end
