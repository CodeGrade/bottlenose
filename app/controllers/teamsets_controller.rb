require 'csv'
require 'teamset_spreadsheet'

class TeamsetsController < ApplicationController
  layout 'course'

  before_action :find_course
  before_action :find_teamset, except: [:index, :investigate, :export]
  before_action :require_registered_user
  before_action :stop_impersonating_user, only: [:edit, :update, :dissolve_all, :randomize, :bulk_enter,
                                                 :accept_request, :reject_request,
                                                 :accept_all_requests, :reject_all_requests, :export
                                                ]
  before_action :require_admin_or_staff, only: [:edit, :update, :dissolve_all, :randomize, :bulk_enter,
                                                :accept_request, :reject_request,
                                                :accept_all_requests, :reject_all_requests,
                                                :investigate, :export
                                               ]

  # GET /staff/courses/:course_id/teams
  def index
    if current_user_site_admin? || current_user_staff_for?(@course)
      @teams = @course.teams.includes(:users).order(end_date: :desc, id: :asc)
      @regs_by_user = @course.registrations.map{|reg| [reg.user_id, reg]}.to_h
      @sections_by_user = RegistrationSection.where(registration: @course.registrations).group_by(&:registration_id)
      @section_crns = @course.sections.map{|sec| [sec.id, sec.crn]}.to_h
      @section_types = @course.sections.map{|sec| [sec.id, sec.type]}.to_h
      @users = @course.users
               .select("users.*", "registrations.dropped_date", "registrations.id as reg_id")
               .map{|s| [s.id, s]}.to_h
      @team_info = @teams.map do |team|
        sections = team.users.map do |user|
          (@sections_by_user[@users[user.id]&.reg_id] || []).map do |sec|
            [@section_crns[sec.section_id], @section_types[sec.section_id]]
          end
        end.flatten(1).uniq.compact
        team_types = team.users.map{|u| @regs_by_user[u.id].role}.uniq.compact
        row_warnings = []
        hazards = sections.group_by(&:second).keep_if{|t, secs| secs.count != 1}.map do |t, secs|
          ["multi_#{t}", "This team spans #{t.pluralize(secs.count)} #{secs.map(&:first).sort.to_sentence}"]
        end
        if team_types.size != 1
          hazards.push(["mixed-team", "Members of this team do not have same role in course"])
          row_warnings.push("error", "mixed-team")
        elsif !team_types.member?("student")
          hazards.push(["staff-team", "Members of this team are not students"])
          row_warnings.push("warning", "staff-team")
        elsif team.active?
          row_warnings.push("success")
        else
          row_warnings.push("inactive")
        end
        [team.id, [sections, hazards, row_warnings]]
      end.to_h
    else
      @teams = current_user.teams.where(course: @course).includes(:users).order(end_date: :desc, id: :asc)
    end
    @teams = multi_group_by(@teams, [:course_id, :teamset_id])
  end

  def edit
    setup_params
  end

  def export
    respond_to do |format|
      format.xlsx { send_data TeamsetSpreadsheet.new(@course).to_xlsx,
                              type: "application/xlsx", filename: "teamsets.xlsx" }
    end
  end
  
  def update
    @team = Team.new(team_params)

    users = (params["users"] || []).map(&:to_i)
    found_users = User.where(id: users).to_a
    if found_users.count != users.count
      setup_params
      missing = (users.to_set - found_users.map(&:id).to_set).to_a
      @team.errors.add(:base, "Could not find all specified users for team: " + missing.to_sentence)
      render :edit, status: 400
      return
    end
    @team.users = found_users

    if @team.save
      redirect_to edit_course_teamset_path(@course, @teamset),
                  notice: "Team #{@team.id} was successfully created."
    else
      setup_params
      @team.errors.full_messages.each do |e|
        @teamset.errors.add(:base, e)
      end
      render :edit, status: 400
    end
  end

  def investigate
    @assignments_to_teamsets = @course.assignments.where(team_subs: true).order(due_date: :desc).map do |a|
      [a.id, [a.name, a.teamset_id]]
    end.to_h
    @teamsets_by_id = {}
    @assignments_to_teamsets.each do |aid, (n, tsid)|
      @teamsets_by_id[tsid] ||= []
      @teamsets_by_id[tsid] << aid
    end
    @assignment_names = @assignments_to_teamsets.map{|aid, (aname, _)| [aid, aname]}.to_h
    @all_partners = @course.all_partners
    @all_users = User.where(id: @all_partners.keys).map{|u| [u.id, u]}.to_h
    @course_teams = @course.teams
    @all_team_users = TeamUser.where(team: @course_teams).group_by(&:team_id).map{|k, v| [k, v.map(&:user_id)]}.to_h
    @all_teams = @course_teams.map do |t|
      users = @all_users.values_at(*@all_team_users[t.id])
      [t.id, {assns: @teamsets_by_id[t.teamset_id],
              from: t.start_date.at_beginning_of_day.iso8601, to: t.end_date&.at_beginning_of_day&.iso8601,
              users: @all_team_users[t.id].sort,
              desc: "Team #{t.id} - #{users.sort_by(&:sort_name).map(&:display_name).to_sentence}",
              ts_id: t.teamset_id}]
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
    csv = CSV.parse(params[:bulk_teams])
    seen = [].to_set
    csv.each do |row|
      row.map(&:strip!)
      row_set = row.to_set
      overlap = seen & row_set
      seen.merge(row_set)
      if !overlap.empty?
        @failure.push "Found duplicate #{'username'.pluralize(overlap.count)} in multiple teams: #{overlap.to_a.to_sentence}"
      end
    end
    if @failure.empty?
      csv.each do |row|
        users = row.map {|un| User.find_by(username: un)}
        if users.any?(&:nil?)
          @failure.push "Could not find all usernames in #{row.to_sentence}"
        else
          in_teams = users.select{|s| @swat[s.id].nil?}
          if in_teams.empty?
            team = Team.new(bulk_params)
            team.users = users
            users.each do |u| @swat[u.id] = nil end
            if team.valid?
              @success.push team
            else
              @failure.push "Could not create team for #{row.to_sentence}:\n\t" +
                            "#{team.errors.full_messages.join('\n\t')}"
            end
          else
            @failure.push "Could not create team for #{row.to_sentence}:\n\t" +
                          "#{in_teams.map(&:username).to_sentence} #{'is'.pluralize(in_teams.count)} already in an active team"
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
        render :edit, status: 500
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
      render :edit, status: 400
    end
  end

  def dissolve_all
    count = @teamset.dissolve_all(DateTime.current, params[:section_id])
    redirect_back fallback_location: course_teamsets_path(@course),
                  notice: "#{pluralize(count, 'team')} dissolved"
  end
  
  def randomize
    begin
      count = @teamset.randomize(params[:random][:size].to_i,
                                 params[:random][:teams_within],
                                 params[:random][:start_date],
                                 params[:random][:end_date])
      redirect_back fallback_location: course_teamsets_path(@course),
                    notice: "#{pluralize(count, 'random team')} created"
    rescue ArgumentError => e
      redirect_back fallback_location: edit_course_teamset_path(@course, @teamset),
                    alert: "Could not create teams: #{e}"
    end
  end

  def clone
    source = Teamset.find_by(id: params[:teamset])
    if source.nil?
      redirect_back fallback_location: course_teamsets_path(@course),
                    alert: "No such teamset"
      return
    end
    count = @teamset.copy_from(source, nil)
    redirect_back fallback_location: course_teamsets_path(@course),
                  notice: "#{pluralize(count, 'team')} copied from #{source.name}"
  end

  def accept_request
    team_info = custom_params
    uids = (team_info.delete("users") || []).map(&:to_i)
    team_info["users"] = User.where(id: uids)
    if uids.length != team_info["users"].length
      missing = uids - (team_info["users"].map(&:id))
      setup_params
      @teamset.errors.add(:base, "Could not find all users for request: missing #{pluralize('id', missing.count)} #{missing.to_sentence}")
      render :edit, status: 400
      return
    end

    @swat = @teamset.users_without_active_team.map{|s| [s.id, s]}.to_h
    in_teams = team_info["users"].select{|s| @swat[s.id].nil?}
    if in_teams.empty?
      @team = Team.new(team_info)
      if @team.save
        redirect_back fallback_location: edit_course_teamset_path(@course, @teamset),
                      notice: "Team #{@team.id} was successfully created."
      else
        setup_params
        @team.errors.full_messages.each do |e|
          @teamset.errors.add(:base, e)
        end
        render :edit, status: 400
      end
    else
      setup_params
      @teamset.errors.add(:base, "Could not create team: #{in_teams.map(&:username).to_sentence} " +
                                 "#{'is'.pluralize(in_teams.count)} already in an active team")
      render :edit, status: 400
    end
  end
  def reject_request
    count = TeamRequest.where(teamset: @teamset, user: custom_params["users"]).delete_all
    redirect_back fallback_location: edit_course_teamset_path(@course, @teamset),
                  notice: "One team request (#{pluralize(count, 'student')}) rejected"
  end
  def accept_all_requests
    team_info = custom_params
    setup_params(params[:section_id])
    @swat = @others.map{|s| [s.id, s]}.to_h
    count = 0
    failure = []
    team = nil
    @team_requests.each do |req, _, _, _|
      in_teams = req.select{|s| @swat[s.id].nil?}
      begin
        if in_teams.empty?
          team = Team.new(team_info)
          team.users = req
          team.save!
          count = count + 1
        else
          failure.push "Could not create team: #{in_teams.map(&:username).to_sentence} " +
                       "#{'is'.pluralize(in_teams.count)} already in an active team"
        end
      rescue Exception => e
        failure.push e
      end
    end
    if failure.empty?
      redirect_back fallback_location: edit_course_teamset_path(@course, @teamset),
                    notice: "#{pluralize(count, 'team')} added"
    else
      setup_params
      failure.each do |msg|
        @teamset.errors.add(:base, msg)
      end
      render :edit, status: 400
    end
  end
  def reject_all_requests
    if params[:section_id]
      setup_params(params[:section_id])
      user_ids = @team_requests.map(&:first).flatten(1).map(&:id)
      count = TeamRequest.where(teamset: @teamset, user_id: user_ids).delete_all
    else
      setup_params
      count = TeamRequest.where(teamset: @teamset).delete_all
    end
    redirect_back fallback_location: edit_course_teamset_path(@course, @teamset),
                  notice: "#{pluralize(@team_requests.count, 'team request')} (#{pluralize(count, 'student')}) rejected"
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

  def custom_params
    ans = params.require(:custom).permit(:start_date, :end_date, users: [])
    ans[:course_id] = params[:course_id]
    ans[:teamset_id] = params[:id]
    ans
  end
    
  def setup_params(section_id = nil)
    @all_teams_for_course = @course.teams.includes(:users)
    @teams = @all_teams_for_course.where(teamset: @teamset).where(Team.active_query, Date.current, Date.current)
    @others = @teamset.students_without_active_team
    @users = @course.users
                .select("users.*", "registrations.dropped_date", "registrations.id as reg_id")
                .map{|s| [s.id, s]}.to_h
    @users_by_username = @users.map do |_, s|
      [s.username, s]
    end.to_h
    @regs_by_user = @course.registrations.map{|reg| [reg.user_id, reg]}.to_h
    @sections_by_user = RegistrationSection.where(registration: @course.registrations).group_by(&:registration_id)
    @others_by_section_ids = {}
    @others.each do |user|
      @sections_by_user[user.reg_id].each do |reg_section|
        @others_by_section_ids[reg_section.section_id] ||= []
        @others_by_section_ids[reg_section.section_id].push user
      end
    end
    @section_crns = @course.sections.map{|sec| [sec.id, sec.crn]}.to_h
    @section_types = @course.sections.map{|sec| [sec.id, sec.type]}.to_h
    @team_requests = []
    # To compute the cliques among team requests, map each student to the set of usernames they requested
    all_requests = @teamset.team_requests.map{|tr| [@users[tr.user_id].username, tr.partners.to_set]}.to_h
    # Ensure each set of partners contain the requesting student's name
    all_requests.each do |un, partners|
      partners << un
    end
    all_requests.each do |un, partners|
      # check for clique by checking for set-equality among all the requested partners
      if partners.all?{|p| all_requests[p] == partners}
        @team_requests << partners.to_a.sort
      end
    end
    @team_requests.uniq!
    @team_requests = @team_requests.map do |names|
      names.map{|name| @users_by_username[name]}
    end
    if section_id
      @team_requests = @team_requests.keep_if do |partners|
        partners.any?{|p| @sections_by_user[p.reg_id].any?{|rs| rs.section_id.to_i == section_id.to_i}}
      end
    end
    @teams_by_user = {}
    @all_teams_for_course.each do |team|
      team.users.each do |u|
        @teams_by_user[u.id] ||= [].to_set
        @teams_by_user[u.id] << team
      end
    end
    @worked_together = {}
    @team_requests.each do |tr|
      tr.each do |u|
        tr.each do |partner|
          next if u == partner
          next unless @teams_by_user[u.id] && @teams_by_user[partner.id]
          common = @teams_by_user[u.id] & @teams_by_user[partner.id]
          next if common.empty?
          @worked_together ||= {}
          @worked_together[u.id] ||= {}
          @worked_together[u.id][partner.id] = common
        end
      end
    end
    @team_requests = @team_requests.map do |tr|
      sections = tr.map do |user|
        (@sections_by_user[@users[user.id]&.reg_id] || []).map do |sec|
          [@section_crns[sec.section_id], @section_types[sec.section_id]]
        end
      end.flatten(1).uniq.compact
      hazards = sections.group_by(&:second).keep_if{|t, secs| secs.count != 1}.map do |t, secs|
        ["multi_#{t}", "This team spans #{t.pluralize(secs.count)} #{secs.map(&:first).sort.to_sentence}"]
      end
      history = []
      tr.combination(2).each do |user, partner|
        if @worked_together.dig(user.id, partner.id)
          history << ["#{[user.display_name, partner.display_name].to_sentence}",
                      investigate_course_teamsets_path(@course, @teamset,
                                                       anchor: "common_partners:student1=#{user.id}:student2=#{partner.id}")]
        end
      end
      [tr, sections, hazards, history]
    end
    @team_info = @teams.map do |team|
      sections = team.users.map do |user|
        (@sections_by_user[@users[user.id]&.reg_id] || []).map do |sec|
          [@section_crns[sec.section_id], @section_types[sec.section_id]]
        end
      end.flatten(1).uniq.compact
      team_types = team.users.map{|u| @regs_by_user[u.id].role}.uniq.compact
      row_warnings = []
      hazards = sections.group_by(&:second).keep_if{|t, secs| secs.count != 1}.map do |t, secs|
        ["multi_#{t}", "This team spans #{t.pluralize(secs.count)} #{secs.map(&:first).sort.to_sentence}"]
      end
      if team_types.size != 1
        hazards.push(["mixed-team", "Members of this team do not have same role in course"])
        row_warnings.push("error", "mixed-team")
      elsif !team_types.member?("student")
        hazards.push(["staff-team", "Members of this team are not students"])
        row_warnings.push("warning", "staff-team")
      elsif team.active?
        row_warnings.push("success")
      else
        row_warnings.push("inactive")
      end
      [team.id, [sections, hazards, row_warnings]]
    end.to_h
  end
end
