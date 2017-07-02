require 'test_helper'

class RegRequestsControllerTest < ActionController::TestCase
  setup do
    make_standard_course
    @mike = create(:user)
    @frank = create(:user)

    @cs301 = @cs101

    @mike_req  = create(:reg_request, user: @mike, course: @cs301, new_sections: [@section.crn])
    @mike_req.save_sections
    @ken_req   = create(:reg_request, user: @ken, course: @cs301, new_sections: [@section.crn])
    @ken_req.save_sections
  end

  test "should get index" do
    skip  # No index page for reg requests

    sign_in @fred
    get :index, params: {course_id: @cs301.id}
    assert_response :success
    assert_not_nil assigns(:reqs)
  end

  test "should get new" do
    sign_in @frank
    get :new, params: {course_id: @cs301.id}
    assert_response :success
  end

  test "should create reg_request" do
    sign_in @frank
    assert_difference('RegRequest.count') do
      post :create, params: {
        course_id: @cs301.id,
        reg_request: {
          new_sections: [@section.crn],
          notes: "Let me in"
        }
      }
    end

    assert_response :redirect
  end

  test "should reject duplicate reg_request" do
    sign_in @mike
    assert_no_difference('RegRequest.count') do
      post :create, params: {
        course_id: @cs301.id,
        reg_request: {
          new_sections: [@section.crn],
          notes: "Let me in"
        }
      }
    end

    assert_response :success
  end

  test "should show reg_request" do
    skip # no :show action for reg_requests -- Users can see them from their user page

    sign_in @fred
    get :show, params: { id: @mike_req.id, course: @cs301 }
    assert_response :success
  end

  test "should destroy reg_request" do

    sign_in @fred
    assert_difference('RegRequest.count', -1) do
      delete :reject, params: {id: @mike_req.id, course_id: @cs301}
    end

    assert_redirected_to course_registrations_path(@cs301)
  end
end
