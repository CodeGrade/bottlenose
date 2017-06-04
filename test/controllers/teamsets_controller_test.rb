require 'test_helper'

class TeamsetsControllerTest < ActionController::TestCase
  setup do
    @team = create(:team)
    @fred = create(:user)
    create(:registration, user: @fred, course: @team.course, section_id: @team.course.sections.first.crn,
           role:  Registration::roles[:professor])

    mreg = create(:registration, course: @team.course, section_id: @team.course.sections.first.crn)
    @mark = mreg.user
    jreg = create(:registration, course: @team.course, section_id: @team.course.sections.first.crn)
    @jane = jreg.user
    greg = create(:registration, course: @team.course, section_id: @team.course.sections.first.crn)
    @greg = greg.user

    @largeCourse = create(:course)
    @largeTs = create(:teamset, course: @largeCourse)
    @manyUsers = (1..30).map do |n| create(:user) end
    @manyUsers.each do |u|
      Registration.create(user: u, course: @largeCourse, section: @largeCourse.sections.first,
                          role: Registration::roles[:student], show_in_lists: true)
    end
    Registration.create(user: @fred, course: @largeCourse, section: @largeCourse.sections.first,
                        role: Registration::roles[:professor], show_in_lists: true)
  end

  test "should get index" do
    sign_in @fred
    get :index, params: { course_id: @team.course, teamset_id: @ts1 }
    assert_response :success
    assert_not_nil assigns(:teams)
  end

  test "should get edit" do
    sign_in @fred
    get :edit, params: { id: @team.teamset, course_id: @team.course }
    assert_response :success
  end

  test "should create team" do
    sign_in @fred

    assert_difference('Team.count') do
      patch :update, params: {
              course_id: @team.course,
              id: @team.teamset_id,
              single: { start_date: @team.start_date },
              users: [ @mark.id, @jane.id, @greg.id ] }
    end

    assert_response :redirect
    assert_equal 3, assigns(:team).users.count
  end

  test "should create random teams" do
    sign_in @fred
    assert_equal 30, @largeCourse.students.count
    assert_equal 0, @largeCourse.teams.count
    assert_equal 1, @largeCourse.teamsets.count
    # From an empty teamset, create random teams of size 3
    assert_difference('Team.count', 10) do
      patch :randomize, params: {
              course_id: @largeCourse.id,
              id: @largeTs.id,
              random: {
                start_date: Date.today,
                end_date: Date.today + 1.week,
                size: 3
              } }
    end
    assert_response :redirect
    assert_equal 10, @largeTs.teams.count
    assert_equal "10 random teams created", flash[:notice]
    # Try it again, and nothing should happen
    assert_no_difference('Team.count') do
      patch :randomize, params: {
              course_id: @largeCourse.id,
              id: @largeTs.id,
              random: {
                start_date: Date.today,
                end_date: Date.today + 1.week,
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
      patch :randomize, params: {
              course_id: @largeCourse.id,
              id: @largeTs.id,
              random: {
                start_date: Date.today,
                end_date: Date.today + 1.week,
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
    assert_difference('Team.count', 8) do
      patch :randomize, params: {
              course_id: @largeCourse.id,
              id: @largeTs.id,
              random: {
                start_date: Date.today,
                end_date: Date.today + 1.week,
                size: 4
              } }
    end
    assert_response :redirect
    # 19 teams from before, plus ceil(30 / 4) more == 27 total
    assert_equal "8 random teams created", flash[:notice]
    assert_equal 27, assigns(:teamset).teams.count
    # Test :dissolve_all
    assert_no_difference('Team.count') do
      patch :dissolve_all, params: {
              course_id: @largeCourse.id,
              id: @largeTs.id
            }
    end
    @largeTs.reload
    assert_equal 0, @largeTs.active_teams.count
  end

  test "should clone teamset" do
    sign_in @fred
    @largeTs.randomize(3, Date.today)
    @largeTs.dissolve_all(DateTime.current)
    @largeTs.randomize(6, Date.today)
    # Create 15 teams, only five of which are active
    @ts2 = create(:teamset, course: @largeCourse)
    assert_difference('Team.count', 5) do
      patch :clone, params: {
              course_id: @largeCourse.id,
              id: @ts2.id,
              teamset: @largeTs.id
            }
    end
    @ts2.reload
    assert_equal 5, @ts2.teams.count
  end
end
