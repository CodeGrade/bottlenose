require 'test_helper'

class AssignmentsControllerTest < ActionController::TestCase
  setup do
    make_standard_course

    @hello = create(:assignment, course: @cs101)
    @bad   = create(:assignment, course: @cs101)
  end

  test "should get new" do
    sign_in @fred
    get :new, {course_id: @cs101.id}
    assert_response :success
  end

  test "should create assignment" do
    sign_in @fred
    assert_difference('Assignment.count') do
      post :create, {
        course_id: @cs101.id,
        assignment: { assignment: "Dance a jig.",
                      points_available: 100,
                      name: "Useful Work",
                      due_date: '2019-05-22',
                      available: '2011-05-22',
                      type: "files",
        },
        lateness: {
          "type" => "lateness_UseCourseDefaultConfig",
        },
        graders: {
          "1477181088065"=> {
            "type"=>"1477181088065_ManualGrader",
            "id"=>"",
            "removed"=>"false",
            "JavaStyleGrader"=>{"avail_score"=>"50"},
            "CheckerGrader"=>{"avail_score"=>"50", "params"=>""},
            "JunitGrader"=>{"avail_score"=>"50", "params"=>""},
            "ManualGrader"=>{"avail_score"=>"50"}},
        }
      }
    end

    assert_redirected_to [@cs101, assigns(:assignment)]
  end

  test "should show assignment" do
    sign_in @john
    get :show, {id: @hello, course_id: @cs101}
    assert_response :success
  end

  test "should get edit" do
    sign_in @fred
    get :edit, {id: @hello, course_id: @cs101}
    assert_response :success
  end

  test "should update assignment" do
    sign_in @fred
    put :update, {
      id: @hello, course_id: @cs101,
      assignment: { 
        assignment: @hello.assignment,
        name: "Something different",
        type: "files",
      },
      lateness: {
        "type" => "lateness_UseCourseDefaultConfig",
      },
      graders: {
        "1477181088065"=> {
          "type"=>"1477181088065_ManualGrader",
          "id"=>"",
          "removed"=>"false",
          "JavaStyleGrader"=>{"avail_score"=>"50"},
          "CheckerGrader"=>{"avail_score"=>"50", "params"=>""},
          "JunitGrader"=>{"avail_score"=>"50", "params"=>""},
          "ManualGrader"=>{"avail_score"=>"50"}},
      }
    }
    assert_redirected_to [@cs101, assigns(:assignment)]
  end

  test "should destroy assignment" do
    sign_in @fred

    assert_difference('Assignment.count', -1) do
        delete :destroy, {id: @bad, course_id: @cs101}
    end

    assert_redirected_to @cs101
  end
end
