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

  test "should show team" do
    sign_in @fred
    get :show, params: { id: @team, course_id: @team.course, teamset_id: @team.teamset_id }
    assert_response :success
  end

  test "should destroy team" do
    skip

    sign_in @fred
    assert_difference('Team.count', -1) do
      delete :destroy, params: { id: @team, course_id: @team.course }
    end

    assert_redirected_to course_teams_path(@team.course)
  end
end
