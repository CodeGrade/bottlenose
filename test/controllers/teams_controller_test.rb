require 'test_helper'

class TeamsControllerTest < ActionController::TestCase
  setup do
    @team = create(:team)
    @fred = create(:user)
    create(:registration, user: @fred, course: @team.course, new_sections: [@team.course.sections.first.crn],
           role:  Registration::roles[:professor]).save_sections

    mreg = create(:registration, course: @team.course, new_sections: [@team.course.sections.first.crn])
    mreg.save_sections
    @mark = mreg.user
    jreg = create(:registration, course: @team.course, new_sections: [@team.course.sections.first.crn])
    jreg.save_sections
    @jane = jreg.user
    greg = create(:registration, course: @team.course, new_sections: [@team.course.sections.first.crn])
    greg.save_sections
    @greg = greg.user
  end

  test "should show team" do
    sign_in @fred
    get :show, params: { id: @team, course_id: @team.course, teamset_id: @team.teamset_id }
    assert_response :success
  end

  test "should dissolve team" do
    sign_in @fred
    assert_nil @team.end_date
    assert_no_difference('Team.count') do
      patch :dissolve, params: { id: @team, course_id: @team.course, teamset_id: @team.teamset_id }
    end

    assert_redirected_to course_teamset_path(@team.course, @team.teamset)
    @team.reload
    assert_not_nil @team.end_date
  end
end
