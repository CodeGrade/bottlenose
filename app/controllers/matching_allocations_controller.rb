require 'csv'
class MatchingAllocationsController < ApplicationController
  layout 'course'
  
  before_action :find_course
  before_action :find_assignment
  before_action :require_current_user
  before_action -> { require_admin_or_prof(course_assignment_path(@course, @assignment)) }

  def edit
    if @assignment.review_target == "self"
      redirect_back fallback_location: course_assignment_path(@course, @assignment),
                    alert: "Cannot specify teams for self-reviews: they must be the same as the underlying submissions."
      return
    end
    @existing_matchings = CodereviewMatching.where(assignment: @assignment)
    @needed_teams = Team.where(id: (@existing_matchings.pluck(:team_id) +
                                    @existing_matchings.pluck(:target_team_id)).compact)
                    .includes(:users).map{|t| [t.id, t]}.to_h
    @needed_users = User.where(id: (@existing_matchings.pluck(:user_id) +
                                    @existing_matchings.pluck(:target_user_id)).compact)
                    .map{|u| [u.id, u]}.to_h

    @targets =
      if @assignment.related_assignment.team_subs?
        by_team = @existing_matchings.group(:target_team_id).count
        @assignment.related_assignment.teamset.active_teams.includes(:users)
          .map{|t| ["(#{by_team[t.id] || 0}) #{t.to_s}", t.id]}
      else
        by_user = @existing_matchings.group(:target_user_id).count
        @course.students.map{|s| ["(#{by_user[s.id] || 0}) #{s.display_name}", s.id]}
      end
    @reviewers =
      if @assignment.team_subs?
        all_teams = @assignment.teamset.active_teams
        full_teams = CodereviewMatching.where(assignment: @assignment, team: all_teams).joins(:team)
                     .group(:team_id).having('count(codereview_matchings.id) >= ?', @assignment.review_count)
                     .pluck(:team_id)
        relevant_teams = all_teams.where.not(id: full_teams)
        relevant_teams.includes(:users).map{|t| [t.to_s, t.id]}
      else
        @course.students.map{|s| [s.display_name, s.id]}
      end
  end
  def patch
    if params[:reviewer].nil?
      redirect_back fallback_location: edit_course_assignment_matchings_path(@course, @assignment),
                    alert: "No reviewers specified"
      return
    elsif params[:target].blank?
      redirect_back fallback_location: edit_course_assignment_matchings_path(@course, @assignment),
                    alert: "No targets specified"
      return
    end
    case [@assignment.team_subs?, @assignment.related_assignment.team_subs?]
    when [true, true]
      reviewer = Team.find_by(id: params[:reviewer])
      if reviewer.nil?
        redirect_back fallback_location: edit_course_assignment_matchings_path(@course, @assignment),
                      alert: "No such reviewer team"
        return
      end
      targets = Team.where(id: params[:target])
      if CodereviewMatching.where(assignment: @assignment, team: reviewer).count == @assignment.review_count
        redirect_back fallback_location: edit_course_assignment_matchings_path(@course, @assignment),
                      alert: "That reviewer already has enough reviews assigned"
        return
      end
      reviewer_users = reviewer.user_ids.to_set
      if targets.any?{|t| !t.user_ids.to_set.disjoint?(reviewer_users)}
        redirect_back fallback_location: edit_course_assignment_matchings_path(@course, @assignment),
                      alert: "One of the reviewers is in one of the reviewee teams"
      else
        targets.each do |t|
          CodereviewMatching.create!(assignment: @assignment,
                                     team: reviewer, target_team: t)
        end
        redirect_to edit_course_assignment_matchings_path(@course, @assignment),
                    notice: "Created #{pluralize(targets.count, 'matching')}"
      end
    when [true, false]
      reviewer = Team.find_by(id: params[:reviewer])
      if reviewer.nil?
        redirect_back fallback_location: edit_course_assignment_matchings_path(@course, @assignment),
                      alert: "No such reviewer team"
        return
      end
      targets = User.where(id: params[:target])
      if CodereviewMatching.where(assignment: @assignment, team: reviewer).count == @assignment.review_count
        redirect_back fallback_location: edit_course_assignment_matchings_path(@course, @assignment),
                      alert: "That reviewer already has enough reviews assigned"
        return
      end
      reviewer_users = reviewer.user_ids.to_set
      if targets.any?{|t| reviewer_users.member?(t)}
        redirect_back fallback_location: edit_course_assignment_matchings_path(@course, @assignment),
                      alert: "One of the reviewers is one of the reviewees"
      else
        targets.each do |t|
          CodereviewMatching.create!(assignment: @assignment,
                                     team: reviewer, target_user: t)
        end
        redirect_to edit_course_assignment_matchings_path(@course, @assignment),
                    notice: "Created #{pluralize(targets.count, 'matching')}"
      end
    when [false, true]
      reviewer = User.find_by(id: params[:reviewer])
      if reviewer.nil?
        redirect_back fallback_location: edit_course_assignment_matchings_path(@course, @assignment),
                      alert: "No such reviewer user"
        return
      end
      targets = Team.where(id: params[:target])
      if CodereviewMatching.where(assignment: @assignment, user: reviewer).count == @assignment.review_count
        redirect_back fallback_location: edit_course_assignment_matchings_path(@course, @assignment),
                      alert: "That reviewer already has enough reviews assigned"
        return
      end
      if targets.any?{|t| t.user_ids.to_set.member?(reviewer.id)}
        redirect_back fallback_location: edit_course_assignment_matchings_path(@course, @assignment),
                      alert: "The reviewers is in one of the reviewee teams"
      else
        targets.each do |t|
          CodereviewMatching.create!(assignment: @assignment,
                                     user: reviewer, target_team: t)
        end
        redirect_to edit_course_assignment_matchings_path(@course, @assignment),
                    notice: "Created #{pluralize(targets.count, 'matching')}"
      end
    when [false, false]
      reviewer = User.find_by(id: params[:reviewer])
      if reviewer.nil?
        redirect_back fallback_location: edit_course_assignment_matchings_path(@course, @assignment),
                      alert: "No such reviewer user"
        return
      end
      targets = User.where(id: params[:target])
      if CodereviewMatching.where(assignment: @assignment, user: reviewer).count == @assignment.review_count
        redirect_back fallback_location: edit_course_assignment_matchings_path(@course, @assignment),
                      alert: "That reviewer already has enough reviews assigned"
        return
      end
      if targets.any?{|t| t.id == reviewer.id}
        redirect_back fallback_location: edit_course_assignment_matchings_path(@course, @assignment),
                      alert: "The reviewer is one of the reviewees"
      else
        targets.each do |t|
          CodereviewMatching.create!(assignment: @assignment,
                                     user: reviewer, target_user: t)
        end
        redirect_to edit_course_assignment_matchings_path(@course, @assignment),
                    notice: "Created #{pluralize(targets.count, 'matching')}"
      end
    end
  end
  def bulk_enter
    @num_added = 0
    @failed = []

    @allTeams = @assignment.teamset.teams
    @allUsers = @course.users
    def find_users_or_fail(ids)
      ids = [ids] unless ids.is_a? Array
      any_dups = ids.group_by{|v| v}.select{|k, v| v.size > 1}.map(&:first)
      unless any_dups.blank?
        @failed << "Found duplicate #{'id'.pluralize(any_dups.count)}: #{any_dups.to_sentence}"
        return nil
      end
      ans = @allUsers.where(id: ids).to_a
      if ans.length != ids.length
        missing = ids.to_set - ans.map(&:id).to_set
        @failed <<
          "Could not find #{'user'.pluralize(missing.count)} with #{'id'.pluralize(missing.count)} #{missing.to_a.to_sentence}"
        return nil
      end
      ans
    end
    def find_teams_or_fail(ids)
      ids = [ids] unless ids.is_a? Array
      any_dups = ids.group_by{|v| v}.select{|k, v| v.size > 1}.map(&:first)
      unless any_dups.blank?
        @failed << "Found duplicate #{'id'.pluralize(any_dups.count)}: #{any_dups.to_sentence}"
        return nil
      end
      ans = @allTeams.where(id: ids).to_a
      if ans.length != ids.length
        missing = ids.to_set - ans.map(&:id).to_set
        @failed <<
          "Could not find #{'team'.pluralize(missing.count)} with #{'id'.pluralize(missing.count)} #{missing.to_a.to_sentence}"
        return nil
      end
      ans
    end
    def save_if_possible(match)
      begin
        match.save!
        @num_added += 1
      rescue ActiveRecord::RecordNotUnique => e
        @failed << "Matching already exists: #{match}"
      end
    end
    CSV.parse(params[:matchings]) do |row|
      nums = row.map{|v| Integer(v) rescue false}
      next unless nums.all?
      case [@assignment.team_subs?, @assignment.related_assignment.team_subs?]
      when [true, true]
        reviewer_team = find_teams_or_fail(nums[0])&.first
        target_teams = find_teams_or_fail(nums[1..-1])
        next if reviewer_team.nil? || target_teams.nil?
        target_teams.each do |tt|
          save_if_possible(CodereviewMatching.new(assignment: @assignment, team: reviewer_team, target_team: tt))
        end
      when [true, false]
        reviewer_team = find_teams_or_fail(nums[0])&.first
        target_users = find_users_or_fail(nums[1..-1])
        next if reviewer_team.nil? || target_users.nil?
        target_users.each do |tu|
          save_if_possible(CodereviewMatching.new(assignment: @assignment, team: reviewer_team, target_user: tu))
        end
      when [false, true]
        reviewer_user = find_users_or_fail(nums[0])&.first
        target_teams = find_teams_or_fail(nums[1..-1])
        next if reviewer_user.nil? || target_teams.nil?
        target_teams.each do |tt|
          save_if_possible(CodereviewMatching.new(assignment: @assignment, user: reviewer_user, target_team: tt))
        end
      when [false, false]
        reviewer_user = find_users_or_fail(nums[0])&.first
        target_users = find_users_or_fail(nums[1..-1])
        next if reviewer_user.nil? || target_users.nil?
        target_users.each do |tu|
          save_if_possible(CodereviewMatching.new(assignment: @assignment, user: reviewer_user, target_user: tu))
        end
      end
    end
    if @failed.blank?
      redirect_to edit_course_assignment_matchings_path(@course, @assignment),
                  notice: "Added #{pluralize(@num_added, 'matching')}."
    else
      @failed.each do |f| @assignment.errors.add(:base, f) end
      redirect_to edit_course_assignment_matchings_path(@course, @assignment),
                  notice: "Added #{pluralize(@num_added, 'matching')}.",
                  alert: "Could not add #{pluralize(@failed.count, 'matching')}: #{@failed.to_sentence}"
    end
  end
  def update
    CodereviewMatching.transaction do
      case [@assignment.team_subs?, @assignment.related_assignment.team_subs?]
      when [true, true]
        reviewer_teams = @assignment.teamset.active_teams.to_a
        target_teams = @assignment.related_assignment.teamset.active_teams.to_a
        @count = general_update(
          reviewer_teams, target_teams,
          lambda {|rev_team| rev_team.user_ids.to_set},
          lambda {|tar_team| tar_team.user_ids.to_set},
          lambda {|rev_user_ids, tar_user_ids| tar_user_ids.disjoint?(rev_user_ids)},
          lambda {|rev, tar| CodereviewMatching.create!(assignment: @assignment, team: rev, target_team: tar)})
        
      when [true, false]
        reviewer_teams = @assignment.teamset.active_teams.to_a
        target_users = @course.students.to_a
        @count = general_update(
          reviewer_teams, target_users,
          lambda {|rev_team| rev_team.user_ids.to_set},
          lambda {|tar_user| tar_user.id},
          lambda {|rev_user_ids, tar_user_id| !rev_user_ids.member?(tar_user_id)},
          lambda {|rev, tar| CodereviewMatching.create!(assignment: @assignment, team: rev, target_user: tar)})
        
      when [false, true]
        reviewer_users = @course.students.to_a
        target_teams = @assignment.related_assignment.teamset.active_teams.to_a
        @count = general_update(
          reviewer_users, target_teams,
          lambda {|rev_user| rev_user.id},
          lambda {|tar_team| tar_team.user_ids.to_set},
          lambda {|rev_user_id, tar_user_ids| !tar_user_ids.member?(rev_user_id)},
          lambda {|rev, tar| CodereviewMatching.create!(assignment: @assignment, user: rev, target_team: tar)})
        
      when [false, false]
        reviewer_users = @course.students.to_a
        target_users = @course.students.to_a
        @count = general_update(
          reviewer_users, target_users,
          lambda {|rev_user| rev_user.id},
          lambda {|tar_user| tar_user.id},
          lambda {|rev_user_id, tar_user_id| rev_user_id != tar_user_id},
          lambda {|rev, tar| CodereviewMatching.create!(assignment: @assignment, user: rev, target_user: tar)})
        
      end
    end
    redirect_to edit_course_assignment_matchings_path(@course, @assignment),
                notice: "#{@count} matchings created"
  end
  def delete
    m = CodereviewMatching.find_by(id: params[:id])
    if m.nil?
      redirect_back fallback_location: edit_course_assignment_matchings_path(@course, @assignment),
                    alert: "No such matching found"
      return
    end
    if m.assignment_id == @assignment.id
      m.delete
      redirect_to edit_course_assignment_matchings_path(@course, @assignment),
                  notice: "1 matching deleted"
    else
      redirect_back fallback_location: edit_course_assignment_matchings_path(@course, @assignment),
                    alert: "That matching does not belong to this assignment"
    end      
  end
  def delete_all
    matchings = @assignment.codereview_matchings.destroy_all
    redirect_back fallback_location: edit_course_assignment_matchings_path(@course, @assignment),
                  notice: "#{pluralize(matchings.count, 'matching')} deleted"
  end

  private
  def general_update(reviewers, targets, get_reviewer_users, get_target_users, disjoint_users, create_matching)
    review_counts = targets.map{|t| [t.id, 0]}.to_h
    targets_ids_map = targets.map{|t| [t.id, t]}.to_h
    target_ids = review_counts.keys
    target_ids.shuffle!
    @count = 0
    reviewers.each do |r|
      r_users = get_reviewer_users.call(r)
      assoc = []
      target_ids.each do |tid|
        t = targets_ids_map[tid]
        break if assoc.length == @assignment.review_count
        next if review_counts[t.id].nil?
        if disjoint_users.call(r_users, get_target_users.call(t))
          assoc << t
          review_counts[t.id] += 1
          if review_counts[t.id] == @assignment.review_count
            review_counts.delete(t.id)
          end
        end
      end
      assoc.each do |a|
        create_matching.call(r, a)
        @count += 1
      end
    end
    return @count
  end
end
