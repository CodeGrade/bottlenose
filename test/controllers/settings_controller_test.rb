require 'test_helper'

class SettingsControllerTest < ActionController::TestCase
  setup do
    @admin = create(:admin_user)
    @prof  = create(:user)
  end

  test "non-admin should not get settings" do
    sign_in @prof
    get :edit
    assert_response :redirect
  end

  test "index should show defaults" do
    Settings.clear_test!

    sign_in @admin
    get :edit
    assert_response :success
    assert_match "noreply@example.com", @response.body
  end

  test "should save_settings" do
    sign_in @admin
    post :update, params: { site_email: "somebody@example.com", backup_login: "" }

    assert_response :redirect
    assert_match "Settings Saved", flash[:notice]
    assert_equal Settings['site_email'], "somebody@example.com"
  end
end
