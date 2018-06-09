class TeamsController < ApplicationController
  layout 'course'

  before_action :find_course
  before_action :find_teamset
  before_action :find_team
  before_action :require_registered_user
  before_action :require_admin_or_staff, only: [:dissolve]

  # GET /courses/:course_id/teams/:id
  def show
    if !(current_user_site_admin? || current_user_staff_for?(@course)) && @team.users.exclude?(current_user)
      redirect_to(root_path, alert: "You are not a member of that team.")
    end
  end

  def dissolve
    @team.dissolve(DateTime.current)
    redirect_back fallback_location: course_teamset_path(@course, @teamset),
                  notice: "Team was successfully dissolved"
  end

  private

  def find_teamset
    @teamset = Teamset.find_by(id: params[:teamset_id])
    if @teamset.nil?
      redirect_back fallback_location: course_teamsets_path(@course),
                    alert: "No such teamset"
      return
    end
  end
  def find_team
    @team = Team.find_by(id: params[:id])
    if @team.nil? || @team.teamset_id != @teamset.id
      redirect_back fallback_location: course_teams_path(@course),
                    alert: "No such team for this teamset"
      return
    end
  end
end
