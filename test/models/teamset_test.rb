require 'test_helper'

class TeamsetTest < ActiveSupport::TestCase

    setup do 
        make_standard_course
    end

    test "Teamset cannot be duplicated if student is not in a course." do
        # Testing Teamset.dub 
        @john_sarah_team_dup = Team.create(users: [@john, @sarah], teamset: @ts1, course: @cs101, 
                                            start_date: DateTime.yesterday, end_date: nil)
        @john_sarah_team_dup.save!

        @john_reg.dropped_date = DateTime.current
        @john_reg.save
        
        assert_raises ActiveTeamWithDroppedStudent do
            @duped_ts = @ts1.dup
        end
    end

    test "Teamset cannot be copied from another Teamset with active teams that contain a dropped student." do
        # Testing Teamset.copy_from
        @js_team_copy_from = Team.create(users: [@john, @sarah], teamset: @ts1, course: @cs101, 
                                            start_date: DateTime.yesterday, end_date: nil)
        @js_team_copy_from.save

        @john_reg.dropped_date = DateTime.current
        @john_reg.save

        assert_raises ActiveTeamWithDroppedStudent do
            @copied_from_ts1 = Teamset.create(course: @cs101)
            @copied_from_ts1.save
            @copied_from_ts1.copy_from(@ts1, false)
        end        
    end

end
