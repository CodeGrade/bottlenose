require 'test_helper'

class TeamsetsTest < ActionDispatch::IntegrationTest
  include Devise::Test::IntegrationHelpers
  setup do
    DatabaseCleaner.clean
    Capybara.current_driver = :webkit
    @team = create(:team)
    @fred = create(:user)
    r = create(:registration, user: @fred, course: @team.course, new_sections: [@team.course.sections.first],
               role:  Registration::roles[:professor])
    r.save_sections

    mreg = create(:registration, course: @team.course, new_sections: [@team.course.sections.first])
    mreg.save_sections
    @mark = mreg.user
    jreg = create(:registration, course: @team.course, new_sections: [@team.course.sections.first])
    jreg.save_sections
    @jane = jreg.user
    greg = create(:registration, course: @team.course, new_sections: [@team.course.sections.first])
    greg.save_sections
    @greg = greg.user

    @largeCourse = create(:course)
    @largeTs = create(:teamset, course: @largeCourse)
    @manyUsers = (1..30).map do |n| create(:user) end
    @manyUsers.each do |u|
      r = Registration.create(user: u, course: @largeCourse, new_sections: [@largeCourse.sections.first],
                          role: Registration::roles[:student], show_in_lists: true)
      r.save_sections
      r
    end
    Registration.create(user: @fred, course: @largeCourse, new_sections: [@largeCourse.sections.first],
                        role: Registration::roles[:professor], show_in_lists: true)
      .save_sections
  end

  test "should create random teams" do
    sign_in @fred
    assert_equal 30, @largeCourse.students.count
    assert_equal 0, @largeCourse.teams.count
    assert_equal 1, @largeCourse.teamsets.count
    # From an empty teamset, create random teams of size 3
    assert_difference('Team.count', 10) do
      patch randomize_course_teamset_path(course_id: @largeCourse.id, id: @largeTs.id), params: {
              random: {
                start_date: Date.today,
                end_date: Date.today + 1.week,
                teams_within: "course",
                size: 3
              } }
    end
    assert_response :redirect
    assert_equal 10, @largeTs.teams.count
    assert_equal "10 random teams created", flash[:notice]
    # Try it again, and nothing should happen
    assert_no_difference('Team.count') do
      patch randomize_course_teamset_path(course_id: @largeCourse.id, id: @largeTs.id), params: {
              random: {
                start_date: Date.today,
                end_date: Date.today + 1.week,
                teams_within: "course",
                size: 3
              } }
    end
    assert_response :redirect
    @largeTs.reload
    assert_equal 10, assigns(:teamset).teams.count
    assert_equal "0 random teams created", flash[:notice]
    # Dissolve a few teams (6 teams * 3 partners == 18 people) and try again
    @largeTs.active_teams.take(6).each do |t| t.dissolve(DateTime.now) end
    assert_difference('Team.count', 9) do
      patch randomize_course_teamset_path(course_id: @largeCourse.id, id: @largeTs.id), params: {
              random: {
                start_date: Date.today,
                end_date: Date.today + 1.week,
                teams_within: "course",
                size: 2
              } }
    end
    assert_response :redirect
    @largeTs.reload
    assert_equal 19, @largeTs.teams.count
    assert_equal "9 random teams created", flash[:notice]
    # Manually dissolve all teams
    assert_equal 13, @largeTs.active_teams.count
    @largeTs.dissolve_all(DateTime.now)
    assert_equal 0, @largeTs.active_teams.count
    assert_difference('Team.count', 7) do
      patch randomize_course_teamset_path(course_id: @largeCourse.id, id: @largeTs.id), params: {
              random: {
                start_date: Date.today,
                end_date: Date.today + 1.week,
                teams_within: "course",
                size: 4
              } }
    end
    assert_response :redirect
    # 19 teams from before, plus floor(30 / 4) more == 26 total
    assert_equal "7 random teams created", flash[:notice]
    assert_equal 26, assigns(:teamset).teams.count
    # Test :dissolve_all
    assert_no_difference('Team.count') do
      patch dissolve_all_course_teamset_path(course_id: @largeCourse.id, id: @largeTs.id)
    end
    @largeTs.reload
    assert_equal 0, @largeTs.active_teams.count
  end
end
