require 'test_helper'

class LoginRedirectTest < ActionDispatch::IntegrationTest
  include Devise::Test::IntegrationHelpers

  setup do
    make_standard_course
  end

  test "redirect to intended page after login" do
    get '/users'
    assert_redirected_to root_path(next: "/users")
    @fred.update_attribute(:sign_in_count, 2)
    sign_in @fred
    assert_equal users_path, request.fullpath
  end
end
