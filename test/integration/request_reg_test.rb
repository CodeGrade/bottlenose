require 'test_helper'

class RequestRegTest < ActionDispatch::IntegrationTest
  setup do
    make_standard_course
  end

  test "request and create a registration" do
    skip

    # Register a new account
    visit "http://test.host/"

    within "#register-form-div" do
      fill_in "Full Name", with: "Napoleon Bonaparte"
      fill_in "Email", with: "napolean@example.com"
      click_button "Register"
    end

    assert has_content?("User created")

    user = User.find_by_email("napolean@example.com")
    visit "http://test.host/main/auth?email=#{user.email}&key=#{user.auth_key}"

    click_link "Your Courses"
    click_link @cs101.name
    click_link "Request Registration"

    fill_in "Notes", with: "I demand class access!"
    click_button "Request Registration"

    # Verify that the request exists
    req = RegRequest.find_by_user_id(user.id)
    assert_equal req.name, "Napoleon Bonaparte"
    assert_equal req.course_id, @cs101.id

    # As a professor, accept the request.
    visit "http://test.host/main/auth?email=#{@fred.email}&key=#{@fred.auth_key}"
    click_link "Your Courses"
    click_link @cs101.name
    first(:link, "View Registration Requests").click

    within "#reg-req-#{req.id}" do
      click_button "Create Registration"
    end

    # Verify that the registration has been created.
    user = User.find_by_email("napolean@example.com")
    assert_equal user.name, "Napoleon Bonaparte"

    reg  = Registration.find_by_user_id_and_course_id(user.id, @cs101.id)
    assert_not_nil reg
  end
end
