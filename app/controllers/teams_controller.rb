class TeamsController < ApplicationController
  layout 'course'

  before_action :find_course
  before_action :find_teamset
  before_action :require_registered_user
  before_action :require_admin_or_staff, only: [:dissolve]

  # GET /courses/:course_id/teams/:id
  def show
    @team = Team.find(params[:id])

    if !(current_user_site_admin? || current_user_staff_for?(@course)) && @team.users.exclude?(current_user)
      redirect_to(root_path, alert: "You are not a member of that team.")
    end
  end

  def dissolve
    @team = Team.find(params[:id])
    @team.dissolve(DateTime.current)
    redirect_back fallback_location: course_teamsets_path(@course, @teamset),
                  notice: "Team was successfully dissolved"
  end

  private

  def find_teamset
    @teamset = Teamset.find(params[:teamset_id])
  end
end
