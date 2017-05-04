require 'test_helper'

class UserTest < ActiveSupport::TestCase
  test "email addresses are forced to lowercase" do
    bob = create(:user, email: "Bob@example.com")
    assert_equal(bob.email, bob.email.downcase)
  end
end
