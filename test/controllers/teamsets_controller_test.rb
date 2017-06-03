require 'test_helper'

class TeamsetsControllerTest < ActionController::TestCase
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
    get :index, params: { course_id: @team.course, teamset_id: @ts1 }
    assert_response :success
    assert_not_nil assigns(:teams)
  end

  test "should create team" do
    sign_in @fred
    assert_difference('Team.count') do
      patch :update, params: {
             course_id: @team.course,
             id: @team.teamset_id,
             single: { course_id: @team.course.id, start_date: @team.start_date },
             users: [ @mark.id, @jane.id, @greg.id ] }
    end

    assert_response :redirect
    assert_equal assigns(:team).users.count, 3
  end


  test "should get edit" do
    skip

    sign_in @fred
    get :edit, params: { id: @team, course_id: @team.course }
    assert_response :success
  end

  test "should update team" do
    skip

    sign_in @fred
    patch :update, params: {
            id: @team, course_id: @team.course,
            team: { course_id: @team.course_id, start_date: @team.start_date },
            users: [ @mark.id ] }
    assert_equal assigns(:team).users.count, 1
    assert_redirected_to course_team_path(@team.course, assigns(:team))
  end
end
