require 'test_helper'

class TeamTest < ActiveSupport::TestCase
  setup do
    make_standard_course
    @jane = create(:user, name: "Jane von Classenstein", first_name: "Jane", last_name: "von Classenstein")
    @jane_reg = create(:registration, course: @cs101, user: @jane, new_sections: [@section.crn])
    @jane_reg.save_sections
    @team = create(:team, teamset: @ts1, course: @cs101, start_date: Date.current)
    @team.users = [@jane, @john]
    @team.save
  end
    
  test "Dissolve team with nil end_date" do
    assert @team.end_date.nil?
    @when = Date.current
    @team.dissolve(@when)
    assert_equal @when.to_date, @team.end_date
  end
  test "Dissolve team with future end_date" do
    @when = Date.current
    @team.end_date = (@when + 1.day)
    assert !@team.end_date.nil?
    @team.dissolve(@when)
    assert_equal @when.to_date, @team.end_date
  end
  test "Dissolve team with expired end_date" do
    @when = Date.current
    @team.end_date = (@when - 1.day)
    assert !@team.end_date.nil?
    @team.dissolve(@when)
    assert_equal @when.to_date - 1.day, @team.end_date
  end
end
