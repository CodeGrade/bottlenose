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
    @teams = @teamset.teams.includes(:users).select(&:active?)
    @others = students_without_active_team
  end

  def update
    @team = Team.new(team_params)

    users = params["users"] || []
    @team.users = users.map { |user_id| User.find(user_id.to_i) }

    if @team.save
      redirect_back fallback_location: edit_course_teamset_path(@course, @teamset),
        notice: "Team #{@team.id} was successfully created."
    else
      @teams = @course.teams.select(&:active?)
      @others = students_without_active_team
      @team.errors.full_messages.each do |e|
        @teamset.errors.add(:base, e)
      end
      render :edit
    end
  end

  def dissolve_all
    count = @teamset.dissolve_all(DateTime.current)
    redirect_back fallback_location: course_teamsets_path(@course, @teamset),
                  notice: "#{pluralize(count, 'team')} dissolved"
  end
  
  def randomize
    count = 0
    students_without_active_team.to_a.shuffle.each_slice(params[:random][:size].to_i).each do |t|
      @team = Team.new(course: @course,
                       start_date: params[:random][:start_date],
                       end_date: params[:random][:end_date],
                       teamset: @teamset
                      )
      @team.users = t

      if @team.save
        count += 1
      end
    end
    redirect_back fallback_location: course_teamsets_path(@course, @teamset),
                  notice: "#{pluralize(count, 'random team')} created"
  end

  def clone
    source = Teamset.find(params[:teamset])
    count = @teamset.copy_from(source, nil)
    redirect_back fallback_location: course_teamsets_path(@course, @teamset),
                  notice: "#{pluralize(count, 'team')} copied from #{source.name}"
  end
  
  private

  def find_teamset
    @teamset = Teamset.find(params[:id])
  end
  
  def students_without_active_team
    @swdi = @course.students_with_drop_info
    @relevant_teams = Team
                      .joins(:team_users)
                      .select("teams.*", "team_users.user_id as uid")
                      .where("team_users.user_id IN (?)", @swdi.pluck(:id))
                      .group_by(&:uid)
    @swdi.where("registrations.dropped_date": nil).reject do |student|
      @relevant_teams[student.id]&.any? do |t| t.teamset_id == @teamset.id && t.active? end
    end
  end


  def team_params
    ans = params.require(:single).permit(:start_date, :end_date)
    ans[:course_id] = params[:course_id]
    ans[:teamset_id] = params[:id]
    ans
  end
end
