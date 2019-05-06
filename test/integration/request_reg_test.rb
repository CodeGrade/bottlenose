require 'test_helper'

class RequestRegTest < ActionDispatch::IntegrationTest
  include Devise::Test::IntegrationHelpers
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
    assert_equal "Napoleon Bonaparte", req.name
    assert_equal @cs101.id, req.course_id

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
    assert_equal "Napoleon Bonaparte", user.name

    reg  = Registration.find_by_user_id_and_course_id(user.id, @cs101.id)
    assert_not_nil reg
  end

  test "registration via request after adding section" do
    sign_in @john
    new_section = Section.new(course: @cs101,
                              crn: 23456,
                              meeting_time: "F 1:35pm",
                              instructor: @fred,
                              type: "lecture")
    @cs101.sections << new_section
    assert_not new_section.students.include? @john
    post course_reg_requests_path @cs101, params: {
        reg_request: {
            role: "student",
            notes: "",
            lecture_sections: new_section.crn.to_s
        }
    }
    sign_in @fred
    delete accept_course_reg_request_path @cs101, RegRequest.find_by(user: @john)
    assert new_section.students.include? @john
  end
end
