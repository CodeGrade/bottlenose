require 'test_helper'

class UsersControllerTest < ActionController::TestCase
  setup do
    @admin = create(:admin_user)
    @user  = create(:user)
    @jack  = create(:user)
  end

  test "should get index" do
    sign_in @admin
    get :index
    assert_response :success
    assert_not_nil assigns(:users)
  end

  test "non admin should not get index" do
    sign_in @user
    get :index
    assert_response :redirect
    assert_match "Must be an admin", flash[:alert]
  end

  test "should get new" do
    skip

    sign_in @admin
    get :new
    assert_response :success
  end

  test "should create user" do
    skip

    sign_in @admin
    assert_difference('User.count') do
      post :create, params: {user: { email: "bob@dole.com", name: "Bob Dole", auth_key: "derp" }}
    end

    assert_redirected_to user_path(assigns(:user))
  end

  test "should show user" do
    sign_in @admin
    get :show, params: {id: @user}
    assert_response :success
  end

  test "should get edit" do
    sign_in @admin
    get :edit, params: {id: @user}
    assert_response :success
  end

  test "should update user" do
    sign_in @admin
    put :update, params: {id: @user, user: { email: @user.email, name: @user.name }}
    assert_redirected_to user_path(assigns(:user))
  end

  test "should destroy user" do
    skip

    sign_in @admin
    assert_difference('User.count', -1) do
      delete :destroy, params: {id: @jack}
    end

    assert_redirected_to users_path
  end
end
