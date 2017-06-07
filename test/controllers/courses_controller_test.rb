require 'test_helper'

class CoursesControllerTest < ActionController::TestCase
  setup do
    make_standard_course
    @mike  = create(:user)

    @course1 = @cs101
    @course2 = create(:course)

    @term = @course1.term
  end

  test "should get index" do
    sign_in @john
    get :index
    assert_response :success
    assert_not_nil assigns(:courses_by_term), "Actually got courses"
  end

  test "non-enrolled user should not have access to course main page" do
    sign_in @mike
    get :show, params: {id: @course1}
    assert_response :redirect
  end

  test "should get new" do
    sign_in @ken
    get :new
    assert_response :success
  end

  test "should create course" do
    lateness = create(:lateness_config)

    sign_in @ken
    assert_difference('Course.count') do
      post :create, params: {
             course: {
               name: "Worst Course Ever", 
               term_id: @term.id,
               sections_attributes: {
                 "0" => {
                   prof_name: @fred.username,
                   crn: "999",
                   meeting_time: "TBD",
                 }
               },
               lateness_config_attributes: {
                 type: "FixedDaysConfig",
                 days_per_assignment: 1,
               }
             }
           }
    end

    assert_redirected_to course_path(assigns(:course))
  end

  test "should show course" do
    sign_in @john
    get :show, params: {id: @course1}
    assert_response :success
  end

  test "should show course with assignments, with no submissions" do
    sign_in @fred

    lateness = create(:lateness_config, type: "FixedDaysConfig", days_per_assignment: 1)
    gc = create(:grader, type: "ManualGrader", avail_score: 50)
    ts = create(:teamset, name: "Teamset for dummy assignment")
    a1 = create(:assignment,
                course: @course1,
                due_date: (Time.now + 5.days),
                name: "Dummy assignment",
                lateness_config: lateness,
                teamset: ts
               )
    get :show, params: {id: @course1}
    assert_response :success
  end

  test "should get edit" do
    sign_in @fred
    get :edit, params: {id: @course1}
    assert_response :success
  end

  test "should update course" do
    sign_in @fred
    put :update, params: {id: @course1, course: { name: @course1.name, footer: "new footer" }}
    assert_response :redirect
  end

  test "updating late penalties should change scores" do
    skip # refers to teacher_scores, rather than the newer score model

    a1  = create(:assignment, course: @course1, due_date: (Time.now - 5.days), teamset: @ts1)
    sub = create(:submission, assignment: a1, user: @john, teacher_score: 100)

    sign_in @fred
    put :update, params: {id: @course1, course: { name: @course1.name, late_options: "5,1,12" }}
    assert_response :redirect

    sub.reload

    assert_equal a1.id, sub.assignment_id
    assert_equal 88, sub.score
  end

  test "non-admin should not be able to update course" do
    sign_in @john
    put :update, params: {id: @course1, course: { name: @course1.name }}
    assert_response :redirect
    assert_match "Must be an", flash[:alert]
  end

  test "should destroy course" do
    skip # currently no route to destroying courses (deliberately)

    sign_in @ken
    assert_difference('Course.count', -1) do
      delete :destroy, params: {id: @course2}
    end

    assert_redirected_to courses_path
  end

  test "non-admin should not be able to destroy course" do
    skip # currently no route to destroying courses (deliberately)

    sign_in @john
    assert_difference('Course.count', 0) do
      delete :destroy, params: {id: @course1}
    end

    assert_response :redirect
    assert_match "don't have permission", flash[:error]
  end

  test "should export gradesheet" do
    a1 = create(:assignment, course: @course1, name: "Assignment 1", teamset: @ts1)
    sub1 = create(:submission, assignment: a1, user: @john, score: 20)

    sign_in @fred
    get :gradesheet, params: {id: @course1.id, format: :xlsx}
    assert_response :ok
  end

  test "should get public page if public" do
    skip # the public page still refers to buckets

    get :public, params: { id: @course1 }
    assert_response :ok
  end

  test "should not get public page unless public" do
    get :public, params: { id: @course2 }
    assert_response :redirect
  end
end
