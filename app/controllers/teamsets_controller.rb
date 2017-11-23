require 'csv'

class TeamsetsController < ApplicationController
  layout 'course'

  before_action :find_course
  before_action :find_teamset, except: [:index, :investigate]
  before_action :require_registered_user
  before_action :stop_impersonating_user, only: [:edit, :update, :dissolve_all, :randomize, :bulk_enter]
  before_action :require_admin_or_staff, only: [:edit, :update, :dissolve_all, :randomize, :bulk_enter, :investigate]

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

  def investigate
    @assignments_to_teamsets = @course.assignments.where(team_subs: true).map do |a|
      [a.id, [a.name, a.teamset_id]]
    end.to_h
    @teamsets_by_id = {}
    @assignments_to_teamsets.each do |aid, (n, tsid)|
      @teamsets_by_id[tsid] ||= []
      @teamsets_by_id[tsid] << n
    end
    @all_partners = @course.all_partners
    @all_users = User.where(id: @all_partners.keys).map do |u|
      [u.id, {profile: Upload.upload_path_for(u.profile || 'silhouette.jpg'),
              name: u.display_name, link: user_path(u), sort_name: u.sort_name}]
    end.to_h
    @course_teams = @course.teams
    @all_team_users = TeamUser.where(team: @course_teams).group_by(&:team_id).map{|k, v| [k, v.map(&:user_id)]}.to_h
    @all_teams = @course_teams.map do |t|
      users = @all_users.values_at(*@all_team_users[t.id])
      [t.id, {assignments: @teamsets_by_id[t.teamset_id],
              from: t.start_date.at_beginning_of_day.iso8601, to: t.end_date&.at_beginning_of_day&.iso8601,
              users: @all_team_users[t.id].sort,
              description: "Team #{t.id} - #{users.sort_by{|a| a[:sort_name]}.map{|a| a[:name]}.to_sentence}",
              link: course_teamset_team_path(@course, t.teamset_id, t)}]
    end.to_h
    @active_teams = @course.active_teams.group_by(&:teamset_id).map do |tsid, teams|
      active = {}
      teams.each do |t|
        @all_team_users[t.id].each do |uid|
          active[uid] = t.id
        end
      end
      [tsid, active]
    end.to_h
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
