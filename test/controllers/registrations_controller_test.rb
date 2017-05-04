require 'test_helper'

class RegistrationsControllerTest < ActionController::TestCase
  setup do
    make_standard_course
    @mike = create(:user)

    @cs301 = @cs101
  end

  test "should get index" do
    sign_in @fred
    get :index, params: {course_id: @cs301.id}
    assert_response :success
  end

  test "should not get index unless logged in" do
    get :index, params: {course_id: @cs301.id}
    assert_response :redirect
    assert_match "You need to log in first", flash[:alert]
  end

  test "non-teacher should not get index" do
    sign_in @john
    get :index, params: {course_id: @cs301.id}
    assert_response :redirect
    assert_match "Must be an admin or staff", flash[:alert]
  end

  test "should create registration" do
    skip # fails without a LDAP connection for now

    sign_in @fred
    assert_difference('Registration.count') do
      post :create, params: {
             course_id: @cs301.id,
             user_name: @mike.name,
             user_email: @mike.email,
             registration: { user_id: @mike.id, course_id: @cs301.id }
           }
    end

    assert_response :redirect
  end

  test "should show registration" do
    skip # no :show action for registrations

    sign_in @fred
    get :show, params: {course_id: @cs301.id, id: @john_reg.id}
    assert_response :success
  end

  test "should get edit" do
    skip # no :edit action for registrations

    sign_in @fred
    get :edit, params: {course_id: @cs301.id, id: @john_reg.id}
    assert_response :success
  end

  test "should update registration" do
    skip # no :update action for registrations

    sign_in @fred
    put :update, params: {
          course_id: @cs301.id,
          id: @john_reg.id,
          registration: { user_id: @john.id, course_id: @cs301.id, teacher: true }
        }

    assert_response :redirect
  end

  test "should set tags" do
    skip # no :update action for registrations

    sign_in @fred
    put :update, params: {course_id: @cs301.id, id: @john_reg.id,
      registration: { user_id: @john.id, course_id: @cs301.id, teacher: false, tags: "goat; honors" }}

    assert_response :redirect
    reg = @john.registration_for(@cs301)
    assert reg.tags.split(/\s*;\s*/).include?("honors")
  end

  test "should toggle show-in-reports" do
    sign_in @fred
    post :toggle, params: { format: 'js', course_id: @cs301.id, id: @john_reg.id }
  end

  test "should destroy registration" do
    sign_in @fred
    assert_difference('Registration.count', -1) do
      delete :destroy, params: {course_id: @cs301.id, id: @john_reg.id}
    end

    assert_redirected_to course_registrations_path(@cs301)
  end
end
