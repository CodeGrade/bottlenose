require 'test_helper'

class MainControllerTest < ActionController::TestCase
  setup do
    @alan = create(:user)
    @john = create(:user)
  end

  test "should get index" do
    get :home
    assert_redirected_to new_user_session_path
  end

  test "lost auth form should create admin on fresh install" do
    skip

    ken_email = "ken@example.com"

    User.delete_all
    assert_equal 0, User.count, "should start with no users"

    num_deliveries = ActionMailer::Base.deliveries.size

    post :resend_auth, :email => ken_email

    assert_equal num_deliveries + 1, ActionMailer::Base.deliveries.size,
      "email should be sent"

    assert_equal 1, User.count, "new user should be created"

    ken = User.find_by_email(ken_email)
    assert ken.site_admin?, "new user should be site admin"

    assert_redirected_to root_url
  end

  test "lost auth form should resend auth email" do
    skip

    num_deliveries = ActionMailer::Base.deliveries.size

    post :resend_auth, :email => @alan.email

    assert_equal num_deliveries + 1, ActionMailer::Base.deliveries.size,
      "email should be sent"

    assert_redirected_to root_url
  end

  test "user can log in" do
    skip

    post :auth, {:email => @alan.email, :key => @alan.auth_key}
    assert_match "Logged in", flash[:notice]
    assert_equal @alan.id, session[:user_id]

    assert_response(:success)
  end

  test "John can't log in as Alan" do
    skip

    post :auth, {:email => @alan.email, :key => @john.auth_key}
    assert_match "Authentication failed", flash[:error]
    assert_nil session[:user_id]

    assert_redirected_to '/'
  end

  test "user can log out" do
    skip

    session[:user_id] = @alan.id

    get :logout
    assert_nil session[:user_id]

    assert_redirected_to '/'
  end
end
