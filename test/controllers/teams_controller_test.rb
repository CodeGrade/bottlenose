require 'test_helper'

class TeamsControllerTest < ActionController::TestCase
  setup do
    @team = create(:team)
    @fred = create(:user)
    create(:registration, user: @fred, course: @team.course, role:  Registration::roles[:professor])

    mreg = create(:registration, course: @team.course)
    @mark = mreg.user
    jreg = create(:registration, course: @team.course)
    @jane = jreg.user
    greg = create(:registration, course: @team.course)
    @greg = greg.user
  end

  test "should get index" do
    sign_in @fred
    get :index, { course_id: @team.course }
    assert_response :success
    assert_not_nil assigns(:active_teams)
    assert_not_nil assigns(:inactive_teams)
  end

  test "should get new" do
    sign_in @fred
    get :new, { course_id: @team.course }
    assert_response :success
  end

  test "should create team" do
    sign_in @fred
    assert_difference('Team.count') do
      post :create, { course_id: @team.course,
                      team: { course_id: @team.course.id, start_date: @team.start_date },
                      users: [ @mark.id, @jane.id, @greg.id ] }
    end

    assert_response :redirect
    assert_equal assigns(:team).users.count, 3
  end

  test "should show team" do
    sign_in @fred
    get :show, { id: @team, course_id: @team.course }
    assert_response :success
  end

  test "should get edit" do
    skip

    sign_in @fred
    get :edit, { id: @team, course_id: @team.course }
    assert_response :success
  end

  test "should update team" do
    skip

    sign_in @fred
    patch :update, { id: @team, course_id: @team.course,
                     team: { course_id: @team.course_id, start_date: @team.start_date },
                     users: [ @mark.id ] }
    assert_equal assigns(:team).users.count, 1
    assert_redirected_to course_team_path(@team.course, assigns(:team))
  end

  test "should destroy team" do
    skip

    sign_in @fred
    assert_difference('Team.count', -1) do
      delete :destroy, { id: @team, course_id: @team.course }
    end

    assert_redirected_to course_teams_path(@team.course)
  end
end
