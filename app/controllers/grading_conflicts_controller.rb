class GradingConflictsController < ApplicationController
    
    layout 'course'

    before_action :find_course
    before_action :require_registered_user
    before_action :require_admin_or_prof, only: [:delete, :update]

    def index
        if current_user.course_professor?(@course) || current_user.site_admin
            @grading_conflicts = GradingConflict.where(course: @course)
        elsif current_user.course_assistant?(@course)
            @grading_conflicts = GradingConflict.where(course: @course, staff: current_user)
        else
            @grading_conflicts = GradingConflict.where(course: @course, student: current_user)
        end
    end

    # TODO: Should there be a show method? May not be enough info/data
    # to warrant showing an individual one since it's essentially
    # just a pair of usernames.

    def show
    end

    def new
        @tas_and_graders = Registration.where(course: @course)
            .where(role: Registration.roles[:grader])
            .or(Registration.where(role: Registration.roles[:assistant]))
            .map{|reg| reg.user}
        @students = Registration.where(course: @course, role: Registration.roles[:student])
            .map{|reg| reg.user}
    end

    def create
    end
    
    private

    # TODO: Add param methods
    def grading_conflict_params
    end    
end
