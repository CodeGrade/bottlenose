require 'csv'

class TeamsetsController < ApplicationController
  layout 'course'

  before_action :find_course
  before_action :find_teamset, except: [:index]
  before_action :require_registered_user
  before_action :stop_impersonating_user, only: [:edit, :update, :dissolve_all, :randomize, :bulk_enter]
  before_action :require_admin_or_staff, only: [:edit, :update, :dissolve_all, :randomize, :bulk_enter]

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
    setup_params
  end

  def update
    @team = Team.new(team_params)

    users = params["users"] || []
    @team.users = users.map { |user_id| User.find(user_id.to_i) }

    if @team.save
      redirect_to edit_course_teamset_path(@course, @teamset),
                  notice: "Team #{@team.id} was successfully created."
    else
      setup_params
      @team.errors.full_messages.each do |e|
        @teamset.errors.add(:base, e)
      end
      render :edit
    end
  end

  def bulk_enter
    @success = []
    @failure = []
    @swat = @teamset.users_without_active_team.map{|s| [s.id, s]}.to_h
    CSV.parse(params[:bulk_teams]) do |row|
      row.map(&:strip!)
      users = row.map {|un| User.find_by(username: un)}
      if users.any?(&:nil?)
        @failure.push "Could not find all usernames in #{row.to_sentence}"
      else
        in_teams = users.select{|s| @swat[s.id].nil?}
        if in_teams.empty?
          team = Team.new(bulk_params)
          team.users = users
          if team.valid?
            @success.push team
          else
            @failure.push "Could not create team for #{row.to_sentence}:\n\t#{team.errors.full_messages.join('\n\t')}"
          end
        else
          if in_teams.count == 1
            @failure.push "Could not create team for #{row.to_sentence}:\n\t#{in_teams.first.username} is already in an active team"
          else
            @failure.push "Could not create team for #{row.to_sentence}:\n\t#{in_teams.map(&:username).to_sentence} are already in active teams"
          end
        end
      end
    end
    if @failure.empty?
      begin
        Team.transaction do
          @success.each(&:save!)
        end
      rescue Exception => e
        @teamset.errors.add(:base, "Could not save all teams: #{e}")
        setup_params
        @teamset.bulk_teams = params[:bulk_teams]
        render :edit
        return
      end
      redirect_to edit_course_teamset_path(@course, @teamset),
                  notice: "#{pluralize(@success.count, 'team')} created"
    else
      @failure.each do |msg|
        @teamset.errors.add(:base, msg)
      end
      setup_params
      @teamset.bulk_teams = params[:bulk_teams]
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

  def bulk_params
    ans = params.require(:bulk).permit(:start_date, :end_date)
    ans[:course_id] = params[:course_id]
    ans[:teamset_id] = params[:id]
    ans
  end

  def setup_params
    @teams = @teamset.teams.includes(:users).where(Team.active_query, Date.current, Date.current)
    @others = @teamset.students_without_active_team
    @students = @course.students
                .select("users.*", "registrations.dropped_date", "registrations.id as reg_id")
                .map{|s| [s.id, s]}.to_h
    @sections_by_student = RegistrationSection.where(registration: @course.registrations).group_by(&:registration_id)
  end
end
