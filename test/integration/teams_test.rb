require 'test_helper'

class TeamsTest < ActionDispatch::IntegrationTest
  setup do
    DatabaseCleaner.clean
    Capybara.current_driver = :webkit

    make_standard_course

    @mark = create(:registration, course: @cs101).user
    @jane = create(:registration, course: @cs101).user
    @mary = create(:registration, course: @cs101).user
    @greg = create(:registration, course: @cs101).user

    @pset = create(:assignment, course: @cs101, team_subs: true, teamset: @ts1)
  end

  def test_create_team_submit_and_grade
    skip

    # Create a Team
    visit "/main/auth?email=#{@fred.email}&key=#{@fred.auth_key}"
    assert has_content?("Logged in as #{@fred.name}")

    click_link 'Your Courses'
    click_link @cs101.name

    first(:link, 'Teams').click
    click_link('New Team')

    find('tr', text: @mary.name).find('.add-user-btn').click
    find('tr', text: @greg.name).find('.add-user-btn').click
    click_button("Create Team")

    assert has_content?("Team was successfully created.")
    assert has_content?(@mary.name)
    assert has_no_content?(@mark.name)

    # Submit as a Team Member
    visit "/main/auth?email=#{@mary.email}&key=#{@mary.auth_key}"

    click_link 'Your Courses'
    click_link @cs101.name
    click_link @pset.name
    click_link "New Team Submission"

    fill_in "Student notes", with: "Greg did the comments"
    attach_file 'Upload', assign_upload('HelloWorld', 'john.tar.gz')
    click_button "Create Submission"

    assert has_content?("Team Members")
    assert has_content?(@mary.name)
    assert has_content?(@greg.name)
    assert has_content?("Greg did the comments")

    # Grade the submission.
    visit "/main/auth?email=#{@fred.email}&key=#{@fred.auth_key}"

    click_link 'Your Courses'
    click_link @cs101.name
    click_link @pset.name

    row = find("td", text: @greg.name).find(:xpath, "..")
    row.fill_in("submission[teacher_score]", with: 80)
    row.click_button "Update Submission"

    assert find("td", text: @mary.name).find(:xpath, "..").has_content?("80")

    mreg = @mary.registration_for(@cs101)
    greg = @greg.registration_for(@cs101)
    assert_equal greg.total_score, mreg.total_score
  end
end
