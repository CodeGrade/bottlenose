class TeamRequestsController < ApplicationController
  layout 'course'

  before_action :require_registered_user
  before_action :find_course
  before_action :find_teamset

  def index
    @team_request = TeamRequest.find_or_initialize_by(teamset: @teamset, user: current_user)
  end

  def create
    @team_request = TeamRequest.find_or_initialize_by(teamset: @teamset, user: current_user)
    @team_request.assign_attributes(team_request_params)
    if @team_request.save
      redirect_to course_teamset_team_requests_path(@course, @teamset),
                  notice: "Your team request has been made."
    else
      @existing = TeamRequest.find_by(teamset: @teamset, user: current_user)
      id = @team_request.id # NOTE: this line seems to be needed or else the subsequent .dup doesn't work
      errors = @team_request.errors
      @team_request = @team_request.dup
      @team_request.errors.copy!(errors)
      render :index
    end
  end
  def update
    create
  end

  def destroy
    tr = TeamRequest.find_by(id: params[:id])
    if (tr.user_id == current_user.id) || true_user&.course_staff?(@course)
      tr.destroy
      redirect_to course_teamset_team_requests_path(@course, @teamset),
                  notice: "Team request deleted"
    else
      redirect_to course_teamset_team_requests_path(@course, @teamset),
                  alert: "That's not your team request"
    end
  end

  private

  def find_teamset
    @teamset = Teamset.find_by(id: params[:teamset_id])
    if @teamset.nil? || @course.id != @teamset.course_id
      redirect_to course_teamsets_path(@course), alert: "That teamset is not part of that course"
      return
    end
  end

  def team_request_params
    ans = {
      teamset: @teamset,
      user: current_user,
      partner_names: params[:team_request][:partner_names].gsub(",", ";")
    }
    ans
  end

end
