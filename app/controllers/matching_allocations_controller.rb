class MatchingAllocationsController < CoursesController
  prepend_before_action :find_course_assignment
  before_action :require_current_user
  before_action :require_admin_or_prof

  def edit
    @existing_matchings = CodereviewMatching.where(assignment: @assignment)
    @needed_teams = Team.where(id: (@existing_matchings.pluck(:team_id) +
                                    @existing_matchings.pluck(:target_team_id)).compact)
                    .includes(:users).map{|t| [t.id, t]}.to_h
    @needed_users = User.where(id: (@existing_matchings.pluck(:user_id) +
                                    @existing_matchings.pluck(:target_team_id)).compact)
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
      reviewer = Team.find(params[:reviewer])
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
      reviewer = Team.find(params[:reviewer])
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
      reviewer = User.find(params[:reviewer])
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
      reviewer = User.find(params[:reviewer])
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
  def update
    case [@assignment.team_subs?, @assignment.related_assignment.team_subs?]
    when [true, true]
      reviewer_teams = @assignment.teamset.active_teams.to_a
      target_teams = @assignment.related_assignment.teamset.active_teams.to_a
      review_counts = target_teams.map{|t| [t.id, 0]}.to_h
      target_teams = target_teams.map{|t| [t.id, t]}.to_h
      targets = review_counts.keys
      targets.shuffle!
      reviewer_teams.each do |rt|
        rt_users = rt.user_ids.to_set
        assoc = []
        targets.each do |t|
          t = target_teams[t]
          break if assoc.length == @assignment.review_count
          next if review_counts[t.id].nil?
          if t.user_ids.to_set.disjoint?(rt_users)
            assoc << t
            review_counts[t.id] += 1
            if review_counts[t.id] == @assignment.review_count
              review_counts.delete(t.id)
            end
          end
        end
        assoc.each do |a|
          CodereviewMatching.create!(assignment: @assignment,
                                     team: rt, target_team: a)
        end
      end
      
    when [true, false]
      reviewer_teams = @assignment.teamset.active_teams.to_a
      target_users = @course.students.to_a
      review_counts = target_users.map{|t| [t.id, 0]}.to_h
      target_users = target_users.map{|t| [t.id, t]}.to_h
      targets = review_counts.keys
      targets.shuffle!
      reviewer_teams.each do |rt|
        rt_users = rt.user_ids.to_set
        assoc = []
        targets.each do |t|
          t = target_users[t]
          break if assoc.length == @assignment.review_count
          next if review_counts[t.id].nil?
          if !rt_users.member?(t.id)
            assoc << t
            review_counts[t.id] += 1
            if review_counts[t.id] == @assignment.review_count
              review_counts.delete(t.id)
            end
          end
        end
        assoc.each do |a|
          CodereviewMatching.create!(assignment: @assignment,
                                     team: rt, target_user: a)
        end
      end
      
    when [false, true]
      reviewer_users = @course.students.to_a
      target_teams = @assignment.related_assignment.teamset.active_teams.to_a
      review_counts = target_teams.map{|t| [t.id, 0]}.to_h
      target_teams = target_teams.map{|t| [t.id, t]}.to_h
      targets = review_counts.keys
      targets.shuffle!
      reviewer_users.each do |ru|
        assoc = []
        targets.each do |t|
          t = target_teams[t]
          break if assoc.length == @assignment.review_count
          next if review_counts[t.id].nil?
          if t.user_ids.to_set.member?(ru.id)
            assoc << t
            review_counts[t.id] += 1
            if review_counts[t.id] == @assignment.review_count
              review_counts.delete(t.id)
            end
          end
        end
        assoc.each do |a|
          CodereviewMatching.create!(assignment: @assignment,
                                     user: ru, target_team: a)
        end
      end
      
    when [false, false]
      reviewer_users = @course.students.to_a
      target_users = @course.students.to_a
      review_counts = target_users.map{|t| [t.id, 0]}.to_h
      target_users = target_users.map{|t| [t.id, t]}.to_h
      targets = review_counts.keys
      targets.shuffle!
      reviewer_users.each do |ru|
        assoc = []
        targets.each do |t|
          t = target_users[t]
          break if assoc.length == @assignment.review_count
          next if review_counts[t.id].nil?
          if t.id != ru.id
            assoc << t
            review_counts[t.id] += 1
            if review_counts[t.id] == @assignment.review_count
              review_counts.delete(t.id)
            end
          end
        end
        assoc.each do |a|
          CodereviewMatching.create!(assignment: @assignment,
                                     user: ru, target_user: a)
        end
      end
      
    end
    redirect_to edit_course_assignment_matchings_path(@course, @assignment),
                notice: "#{CodereviewMatching.where(assignment: @assignment).count} matchings created"
  end
  def delete
    m = CodereviewMatching.find(params[:id])
    if m.assignment_id == @assignment.id
      m.delete
      redirect_to edit_course_assignment_matchings_path(@course, @assignment),
                  notice: "1 matching deleted"
    else
      redirect_back fallback_location: edit_course_assignment_matchings_path(@course, @assignment),
                    alert: "That matching does not belong to this assignment"
    end      
  end

  private
  def find_course_assignment
    @course = Course.find_by(id: params[:course_id])
    @assignment = Assignment.find_by(id: params[:assignment_id])
    if @course.nil?
      redirect_back fallback_location: root_path, alert: "No such course"
      return
    end
    if @assignment.nil? or @assignment.course_id != @course.id
      redirect_back fallback_location: course_path(@course), alert: "No such assignment for this course"
      return
    end
  end

  def require_admin_or_prof
    unless current_user_site_admin? || current_user_prof_for?(@course)
      redirect_back fallback_location: course_assignment_path(@course, @assignment),
                    alert: "Must be an admin or professor."
      return
    end
  end
end
