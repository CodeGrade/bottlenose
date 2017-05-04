require 'test_helper'

class AddUserTest < ActionDispatch::IntegrationTest
  setup do
    make_standard_course
  end

  test "add a student" do
    skip

    # Log in as a professor
    visit "/main/auth?email=#{@fred.email}&key=#{@fred.auth_key}"

    assert has_content?("Logged in as #{@fred.name}")

    click_link 'Your Courses'
    click_link @cs101.name
    first(:link, 'Manage Registrations').click

    assert has_content?("Add a Student or Teacher")

    # Add a new student.
    fill_in 'Email', :with => 'steve@example.com'
    fill_in 'Name',  :with => 'Steve McTest'
    click_button 'Create Registration'
    assert has_content?("Registration was successfully created.")

    # Verify that student was added.
    @steve = User.find_by_email('steve@example.com')
    assert_not_nil @steve
    assert_equal 'Steve McTest', @steve.name
    assert_equal 1, @steve.registrations.size
  end
end
