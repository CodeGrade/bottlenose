require 'test_helper'

class LoginRedirectTest < ActionDispatch::IntegrationTest
  include Devise::Test::IntegrationHelpers

  setup do
    make_standard_course
  end

  test "redirect to intended page after login" do
    get '/users'
    assert_redirected_to root_path
    assert_equal session[:next], '/users'
    @fred.update_attribute(:sign_in_count, 2)
    sign_in @fred
    get '/'
    assert_redirected_to '/users'
    assert_nil session[:next]
  end
end
