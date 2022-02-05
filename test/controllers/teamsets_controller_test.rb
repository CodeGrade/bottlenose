require 'test_helper'

class TeamsetsControllerTest < ActionController::TestCase
  setup do
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

  test "should clone teamset" do
    sign_in @fred
    @largeTs.randomize(3, "course", Date.today)
    @largeTs.dissolve_all(DateTime.current)
    @largeTs.randomize(6, "course", Date.today)
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

  test "shouldn't update team with same start and end date" do 
    sign_in @fred
    assert_no_difference('Team.count') do
      patch :update, params: {
              course_id: @team.course,
              id: @team.teamset_id,
              single: { start_date: Date.today, end_date: Date.today },
              users: [ @mark.id, @jane.id, @greg.id ] }
    end
    assert_response :redirect
  end
end
