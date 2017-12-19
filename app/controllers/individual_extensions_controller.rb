class IndividualExtensionsController < ApplicationController
  layout 'course'

  before_action :find_course
  before_action :find_assignment
  before_action :require_current_user
  before_action -> { require_admin_or_prof(course_assignment_path(@course, @assignment)) }

  def edit
    if @assignment.team_subs?
      @existing_extensions = multi_group_by(@assignment.individual_extensions, [:team_id], true)
      @all_potential = @assignment.teamset.active_teams
    else
      @existing_extensions = multi_group_by(@assignment.individual_extensions, [:user_id], true)
      @all_potential = @course.students
    end
  end

  def patch
    if @assignment.team_subs?
      @ext = IndividualExtension.find_or_initialize_by(assignment: @assignment, team_id: params[:team_id])
    else
      @ext = IndividualExtension.find_or_initialize_by(assignment: @assignment, user_id: params[:user_id])
    end
    @ext.assign_attributes(params.permit(:team_id, :user_id, :due_date))
    @ext.save!
    render json: {id: @ext.id, due_date: @ext.due_date,
                  assignment: @ext.assignment_id, team: @ext.team_id, user: @ext.user_id}
  end

  def update
    debugger
  end

  def delete
    respond_to do |f|
      f.json {
        if @assignment.team_subs?
          @ext = IndividualExtension.find_by(assignment: @assignment, team_id: params[:team_id])
        else
          @ext = IndividualExtension.find_by(assignment: @assignment, user_id: params[:user_id])
        end
        if @ext.nil?
          render json: {none_found: true}, status: 409
        else
          @ext.delete
          render json: {deleted: @ext.id}
        end
      }
      f.html {
        render html: "<html><body>#{ext.id} deleted</body></html>"
      }
    end
  end

  def delete_all
    destroyed = @assignment.individual_extensions.destroy_all
    redirect_back fallback_location: edit_course_assignment_extensions_path(@course, @assignment),
                  notice: "#{pluralize(destroyed.count, 'extension')} revoked"
  end
end
