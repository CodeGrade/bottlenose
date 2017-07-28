class TeamsetsController < ApplicationController
  layout 'course'

  before_action :find_course
  before_action :find_teamset, except: [:index]
  before_action :require_registered_user
  before_action :require_admin_or_staff, only: [:edit, :update, :dissolve_all, :randomize]

  # GET /staff/courses/:course_id/teams
  def index
    if current_user_site_admin? || current_user_staff_for?(@course)
      @teams = @course.teams
    else
      @teams = current_user.teams.where(course: @course)
    end
    @teams = multi_group_by(@teams.includes(:users).order(end_date: :desc, id: :asc),
                            [:course_id, :teamset_id])
  end

  def edit
    @teams = @teamset.teams.includes(:users).select(Team.active_query, Date.current, Date.current)
    @others = @teamset.students_without_active_team
  end

  def update
    @team = Team.new(team_params)

    users = params["users"] || []
    @team.users = users.map { |user_id| User.find(user_id.to_i) }

    if @team.save
      redirect_back fallback_location: edit_course_teamset_path(@course, @teamset),
        notice: "Team #{@team.id} was successfully created."
    else
      @teams = @course.teams.select(Team.active_query, Date.current, Date.current)
      @others = @teamset.students_without_active_team
      @team.errors.full_messages.each do |e|
        @teamset.errors.add(:base, e)
      end
      render :edit
    end
  end

  def dissolve_all
    count = @teamset.dissolve_all(DateTime.current)
    redirect_back fallback_location: course_teamsets_path(@course),
                  notice: "#{pluralize(count, 'team')} dissolved"
  end
  
  def randomize
    count = @teamset.randomize(params[:random][:size].to_i,
                               params[:random][:teams_within],
                               params[:random][:start_date],
                               params[:random][:end_date])
    redirect_back fallback_location: course_teamsets_path(@course),
                  notice: "#{pluralize(count, 'random team')} created"
  end

  def clone
    source = Teamset.find(params[:teamset])
    count = @teamset.copy_from(source, nil)
    redirect_back fallback_location: course_teamsets_path(@course),
                  notice: "#{pluralize(count, 'team')} copied from #{source.name}"
  end

  private

  def find_teamset
    @teamset = Teamset.find_by(id: params[:id])
    if @teamset.nil? || @course.id != @teamset.course_id
      redirect_to course_teamsets_path(@course), alert: "That teamset is not part of that course"
      return
    end
  end

  def team_params
    ans = params.require(:single).permit(:start_date, :end_date)
    ans[:course_id] = params[:course_id]
    ans[:teamset_id] = params[:id]
    ans
  end
end
