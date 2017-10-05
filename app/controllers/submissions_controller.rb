require 'tempfile'
require 'audit'

class SubmissionsController < ApplicationController
  layout 'course'
  
  before_action :find_course
  before_action :find_assignment
  before_action -> { find_submission(params[:id]) }, except: [:index, :new, :create, :rerun_grader]
  before_action :require_current_user, only: [:show, :files, :new, :create]
  before_action -> { require_admin_or_staff(course_assignment_submission_path(@course, @assignment, @submission)) },
                only: [:recreate_grade, :rerun_grader, :use_for_grading, :publish]
  before_action -> { require_admin_or_prof(course_assignment_submission_path(@course, @assignment, @submission)) },
                only: [:rescind_lateness, :edit_plagiarism, :update_plagiarism, :split_submission]
  def show
    unless @submission.visible_to?(current_user)
      redirect_to course_assignment_path(@course, @assignment), alert: "That's not your submission."
      return
    end

    self.send("show_#{@assignment.type.capitalize}")
    render "show_#{@assignment.type.underscore}"
  end

  def index
    redirect_to course_assignment_path(@course, @assignment)
  end

  def details
    unless @submission.visible_to?(current_user)
      redirect_to course_assignment_path(@course, @assignment), alert: "That's not your submission."
      return
    end

    self.send("details_#{@assignment.type.capitalize}")
  end

  def new
    @submission = Submission.new(type: @assignment.type + "Sub")
    @submission.assignment_id = @assignment.id
    @submission.user_id = current_user.id

    if @assignment.team_subs?
      @team = current_user.active_team_for(@course, @assignment)

      if @team.nil? && current_user.course_staff?(@course)
        @team = Team.new(course: @course, start_date: DateTime.now, teamset: @assignment.teamset)
        @team.users = [current_user]
        @team.save
      end

      @submission.team = @team
    end

    if current_user.id == true_user&.id
      SubmissionView.find_or_create_by!(user: current_user, assignment: @assignment, team: @team)
    end

    sub_blocked = @assignment.submissions_blocked(current_user, @team)
    if sub_blocked
      if current_user.course_staff?(@course) ||
         (current_user.id != true_user&.id && true_user&.course_staff?(@course))
        # allow submission
      else
        redirect_back fallback_location: course_assignment_path(@course, @assignment),
                      alert: sub_blocked
        return
      end
    end

    self.send("new_#{@assignment.type.capitalize}")
  end

  def create
    asgn_type = @assignment.type
    sub_type = submission_params[:type]
    case [asgn_type, sub_type]
    when ["Files", "FilesSub"] then true
    when ["Questions", "QuestionsSub"] then true
    when ["Exam", "ExamSub"] then true
    when ["Codereview", "CodereviewSub"] then true
    else
      redirect_to course_assignment_path(@course, @assignment),
                  alert: "That submission type (#{sub_type}) does not match the assignment type (#{asgn_type})."
      return
    end
    if @assignment.team_subs?
      @team = current_user.active_team_for(@course, @assignment)
    else
      @team = nil
    end
    sub_blocked = @assignment.submissions_blocked(current_user, @team)
    if sub_blocked
      if current_user.course_staff?(@course) ||
         (current_user.id != true_user&.id && true_user&.course_staff?(@course))
        # allow submission
      else
        redirect_back fallback_location: course_assignment_path(@course, @assignment),
                      alert: sub_blocked
        return
      end
    end

    @submission = Submission.new(submission_params)
    @submission.assignment = @assignment
    @submission.team = @team

    if true_user_staff_for?(@course) || current_user_staff_for?(@course)
      @submission.user ||= current_user
      @submission.ignore_late_penalty = (submission_params[:ignore_late_penalty].to_i > 0)
      if submission_params[:created_at] and !@submission.ignore_late_penalty
        @submission.created_at = submission_params[:created_at]
      end
    else
      @submission.user = current_user
      @submission.ignore_late_penalty = false
    end


    self.send("create_#{@assignment.type.capitalize}")
  end

  def recreate_grade
    @grader = Grader.find(params[:grader_id])
    if @submission.recreate_missing_grade(@grader)
      @submission.compute_grade! if @submission.grade_complete?
      redirect_back fallback_location: course_assignment_submission_path(@course, @assignment, @submission)
    else
      redirect_back fallback_location: course_assignment_submission_path(@course, @assignment, @submission),
                    alert: "Grader already exists; use Regrade to modify it instead"
    end
  end

  def rerun_grader
    @grader = Grader.find(params[:grader_id])
    count = 0
    @assignment.used_submissions.each do |sub|
      @grader.grade(@assignment, sub)
      count += 1
    end
    redirect_back fallback_location: course_assignment_path(@course, @assignment),
                  notice: "Regraded #{@grader.display_type} for #{pluralize(count, 'submission')}"
  end

  def use_for_grading
    @submission.set_used_sub!
    redirect_back fallback_location: course_assignment_submission_path(@course, @assignment, @submission)
  end

  def rescind_lateness
    @submission.update_attribute(:ignore_late_penalty, true)
    @submission.compute_grade!
    redirect_back fallback_location: course_assignment_submission_path(@course, @assignment, @submission)
  end

  def edit_plagiarism
    @max_points = @assignment.graders.map(&:avail_score).sum
  end

  def update_plagiarism
    guilty_students = params.dig(:submission, :penalty)&.to_unsafe_h || {}
    guilty_students = guilty_students.map{|k, v| [k.to_i, v.to_f]}.to_h

    if guilty_students.empty?
      redirect_back fallback_location: edit_plagiarism_course_assignment_submission_path(@course, @assignment, @submission),
                    alert: "You haven't selected any students as being involved"
      return
    end
    comment = params[:comment]
    if comment.to_s.empty?
      redirect_back fallback_location: edit_plagiarism_course_assignment_submission_path(@course, @assignment, @submission),
                    alert: "Must leave some explanatory comment for the students"
      return
    end

    @max_points = @assignment.graders.map(&:avail_score).sum
    guilty_students.each do |id, penalty|
      penaltyPct = (100.0 * penalty.to_f) / @max_score.to_f
      student = User.find(id)
      sub = split_sub(@submission, student, [@submission.score.to_f - penaltyPct, 0].max)
      # Add the penalty comment
      sub_comment = InlineComment.new(
        submission: sub,
        label: "Plagiarism",
        line: 0,
        filename: sub.upload.extracted_path,
        grade_id: current_user.id,
        severity: "error",
        comment: comment,
        weight: penalty,
        suppressed: false,
        title: "",
        info: nil)
      sub_comment.save!
    end
    # Add a comment explaining the split
    sub_comment = InlineComment.new(
      submission: @submission,
      label: "Plagiarized submission",
      line: 0,
      filename: @submission.upload.extracted_path,
      grade_id: current_user.id,
      severity: "info",
      comment: "This submission has been reviewed for plagiarism and regraded accordingly.",
      weight: 0,
      suppressed: false,
      title: "",
      info: nil)
    sub_comment.save
    redirect_to course_assignment_path(@course, @assignment),
                notice: "Submission marked as plagiarized for #{guilty_students.map{|uid, _| User.find(uid).name}.join(', ')}"
  end

  def split_submission
    if @submission.users.count == 1
      redirect_back course_assignment_submission_path(@course, @assignment, @submission),
                    alert: "Submission is not a team-submission; no need to split it"
      return
    end
    @submission.users.each do |student|
      sub = split_sub(@submission, student)
      # Add a comment explaining the split
      sub_comment = InlineComment.new(
        submission: sub,
        label: "Split submission",
        line: 0,
        filename: sub.upload.extracted_path,
        grade_id: current_user.id,
        severity: "info",
        comment: "This is copied from a previous team submission, for individual grading",
        weight: 0,
        suppressed: false,
        title: "",
        info: nil)
      sub_comment.save!
    end
    # Add a comment explaining the split
    sub_comment = InlineComment.new(
      submission: @submission,
      label: "Split submission",
      line: 0,
      filename: @submission.upload.extracted_path,
      grade_id: current_user.id,
      severity: "info",
      comment: "This submission was split, to allow for individual grading",
      weight: 0,
      suppressed: false,
      title: "",
      info: nil)
    sub_comment.save!
    redirect_to course_assignment_path(@course, @assignment),
                notice: "Group submission split for #{@submission.users.map(&:name).join(', ')}"
  end

  def split_sub(orig_sub, for_user, score = nil)
    team = orig_sub.team
    if team
      # Construct a one-use team, so that this student can be graded in isolation
      team = Team.new(
        teamset_id: team.teamset_id,
        course: @course,
        start_date: orig_sub.created_at,
        end_date: orig_sub.created_at)
      team.users = [for_user]
      team.save
    end
    # Create the new submission, reusing the prior submitted file
    sub = Submission.new(
      assignment: @assignment,
      user: for_user,
      time_taken: orig_sub.time_taken,
      created_at: orig_sub.created_at,
      ignore_late_penalty: false,
      score: score || orig_sub.score,
      team: team,
      upload_id: @submission.upload_id,
      type: @submission.type)
    sub.save
    sub.set_used_sub!
    # Copy any grades
    orig_sub.grades.each do |g|
      new_g = g.dup
      new_g.submission = sub
      new_g.save
      g.inline_comments.each do |c|
        new_c = c.dup
        new_c.grade = new_g
        new_c.submission = sub
        new_c.save
      end
    end
    sub
  end

  def publish
    @submission.grades.where(score: nil).each do |g| g.grade(assignment, used) end
    @submission.grades.update_all(:available => true)
    @submission.compute_grade!
    redirect_back fallback_location: course_assignment_submission_path(@course, @assignment, @submission)
  end


  private

  def submission_params
    if true_user_prof_for?(@course) or current_user_prof_for?(@course)
      params[:submission].permit(:assignment_id, :user_id, :student_notes, :type,
                                 :auto_score, :calc_score, :created_at, :updated_at, :upload,
                                 :grading_output, :grading_uid, :team_id,
                                 :teacher_score, :teacher_notes,
                                 :ignore_late_penalty, :upload_file,
                                 :comments_upload_file, :time_taken,
                                 related_subs: [])
    else
      params[:submission].permit(:assignment_id, :user_id, :student_notes, :type,
                                 :upload, :upload_file, :time_taken,
                                 related_subs: [])
    end
  end

  def answers_params
    params[:answers].to_unsafe_h.map{|k, v| [k, array_from_hash(v)]}.to_h
  end

  ######################
  # Assignment types
  # NEW
  def new_Files
    render "new_#{@assignment.type.underscore}"
  end

  def new_Questions
    @questions = @assignment.questions
    if @assignment.related_assignment
      related_sub = @assignment.related_assignment.used_sub_for(current_user)
      if related_sub.nil?
        @submission_files = []
        @submission_dirs = []
      else
        @submission_dirs, @submission_files = related_sub.get_submission_files(current_user)
      end
    else
      @submission_files = []
      @submission_dirs = []
    end
    render "new_#{@assignment.type.underscore}"
  end

  def new_Exam
    unless current_user_site_admin? || current_user_staff_for?(@course)
      redirect_back fallback_location: course_assignment_path(@course, @assignment),
                    alert: "Must be an admin or staff to enter exam grades."
      return
    end
    @grader = @assignment.graders.first
    redirect_to bulk_course_assignment_grader_path(@course, @assignment, @grader)
  end

  def new_Codereview
    if @assignment.review_target == "self"
      @subs_to_review = [@assignment.related_assignment.used_sub_for(current_user)]
      Audit.log("User #{current_user.id} (#{current_user.name}) is viewing the self-eval for assignment #{@assignment.related_assignment.id} and has agreed not to submit further files to it.\n")
    else
      @team = current_user.active_team_for(@course, @assignment)

      if @team.nil? && current_user.course_staff?(@course)
        @team = Team.new(course: @course, start_date: DateTime.now, teamset: @assignment.teamset)
        @team.users = [current_user]
        @team.save
      end

      if @team
        setup_matchings
      end
    end
    @submission.related_subs = @subs_to_review
    @submission_info = @subs_to_review&.map do |s|
      d, f = s.get_submission_files(current_user)
      [d, f, s.id]
    end
    @questions = @assignment.questions
    render "new_#{@assignment.type.underscore}"
  end

  # CREATE
  def create_Files
    if (@submission.save_upload and @submission.save)
      @submission.set_used_sub!
      @submission.create_grades!
      @submission.autograde!
      path = course_assignment_submission_path(@course, @assignment, @submission)
      redirect_to(path, notice: 'Submission was successfully created.')
    else
      @submission.cleanup!
      new_Files
    end
  end

  def create_Questions
    @submission.answers = answers_params["newsub"]

    @submission.related_files = {}

    if @submission.save_upload && @submission.save
      @submission.set_used_sub!
      @submission.autograde!
      path = course_assignment_submission_path(@course, @assignment, @submission)
      redirect_to(path, notice: 'Response was successfully created.')
    else
      @submission.cleanup!
      new_Questions
    end
  end

  def create_Exam
    # No grades are created here, because we shouldn't ever get to this code
    # The grades are configured in the GradesController, in update_exam_grades
  end

  def create_Codereview
    @submission.answers = answers_params
    @submission.related_files = {}
    @submission.related_subs = @submission.related_subs.map do |id| Submission.find(id) end
    @submission.related_subs.each do |sub|
      _, files = sub.get_submission_files(current_user)
      @submission.related_files[sub.id] = files
    end
    if @submission.save_upload && @submission.save
      @submission.set_used_sub!
      @submission.autograde!
      path = course_assignment_submission_path(@course, @assignment, @submission)
      redirect_to(path, notice: 'Response was successfully created.')
    else
      @submission.cleanup!
      @answers = @submission.answers
      @questions = @assignment.questions
      @submission_info = @submission.related_subs.map do |s|
        d, f = s.get_submission_files(current_user)
        [d, f, s.id]
      end
      render "new_#{@assignment.type.underscore}"
    end
  end

  # SHOW
  def show_Files
    @gradesheet = Gradesheet.new(@assignment, [@submission])
    comments = @submission.grade_submission_comments(true) || {}
    comments = comments[@submission.upload.extracted_path.to_s] || []
    @plagiarized = comments.select{|c| c.label == "Plagiarism"}
    @split = comments.any?{|c| c.label == "Split submission"}
  end
  def show_Questions
    @gradesheet = Gradesheet.new(@assignment, [@submission])
    @questions = @assignment.questions
    @answers = YAML.load(File.open(@submission.upload.submission_path))
    if @assignment.related_assignment
      @related_sub = @assignment.related_assignment.used_sub_for(@submission.user)
      if @related_sub.nil?
        @submission_files = []
        @submission_dirs = []
        @answers_are_newer = true
      else
        @submission_dirs, @submission_files = @related_sub.get_submission_files(current_user)
        @answers_are_newer = (@related_sub.created_at < @submission.created_at)
      end
    else
      @submission_files = []
      @submission_dirs = []
      @answers_are_newer = true
    end
    comments = @submission.grade_submission_comments(true) || {}
    comments = comments[@submission.upload.extracted_path.to_s] || []
    @plagiarized = comments.select{|c| c.label == "Plagiarism"}
    @split = comments.any?{|c| c.label == "Split submission"}
  end
  def show_Exam
    @student_info = @course.students.select(:username, :last_name, :first_name, :id)
    @grader = @assignment.graders.first
    @grade = Grade.find_by(grader_id: @grader.id, submission_id: @submission.id)
    @grade_comments = InlineComment.where(submission_id: @submission.id).order(:line).to_a
  end
  def show_Codereview
    @gradesheet = Gradesheet.new(@assignment, [@submission])
    comments = @submission.grade_submission_comments(true) || {}
    comments = comments[@submission.upload.extracted_path.to_s] || []
    @answers = YAML.load(File.open(@submission.upload.submission_path))
    @related_subs = @submission.review_feedbacks.map(&:submission)
    @submission.related_files = {}
    @related_subs.each do |sub|
      _, files = sub.get_submission_files(current_user)
      @submission.related_files[sub.id] = files
    end    
    @plagiarized = comments.select{|c| c.label == "Plagiarism"}
    @split = comments.any?{|c| c.label == "Split submission"}
  end

  # DETAILS
  def details_Files
    respond_to do |f|
      f.html {
        show_hidden = (current_user_site_admin? || current_user_staff_for?(@course))
        @lineCommentsByFile = @submission.grade_line_comments(current_user, show_hidden)
        @sub_comments = @submission.grade_submission_comments(show_hidden)
        @submission_dirs, @submission_files = @submission.get_submission_files(current_user, @lineCommentsByFile, true)
        render "details_files"
      }
      f.text {
        show_hidden = (current_user_site_admin? || current_user_staff_for?(@course))
        comments = @submission.grades
        unless show_hidden
          comments = comments.where(available: true)
        end
        comments = comments.map(&:inline_comments).flatten
        render plain: GradesController.pretty_print_comments(comments)
      }
    end
  end
  def details_Questions
    @questions = @assignment.questions
    @answers = YAML.load(File.open(@submission.upload.submission_path))
    @answers = [[@submission.id.to_s, @answers]].to_h
    if current_user_site_admin? || current_user_staff_for?(@course)
      @grades = @submission.inline_comments
    else
      @grades = @submission.visible_inline_comments
    end

    @show_grades = false

    @grades = @grades.select(:line, :name, :weight, :comment, :user_id).joins(:user).sort_by(&:line).to_a
    @grades = [["grader", @grades&.first&.user&.name],
               [@submission.id.to_s,
                @grades.map{|g| [["index", g.line], ["score", g.weight], ["comment", g.comment]].to_h}]].to_h
    if @assignment.related_assignment
      @related_sub = @assignment.related_assignment.used_sub_for(@submission.user)
      if @related_sub.nil?
        @submission_files = []
        @submission_dirs = []
        @answers_are_newer = true
      else
        @submission_dirs, @submission_files = @related_sub.get_submission_files(current_user)
        @answers_are_newer = (@related_sub.created_at < @submission.created_at)
      end
    else
      @submission_files = []
      @submission_dirs = []
      @answers_are_newer = true
    end
    render "details_questions"
  end
  def details_Exam
    redirect_back fallback_location: course_assignment_submission_path(@course, @assignment, @submission),
                  alert: "No more detailed information about this exam"
  end
  def details_Codereview
    @questions = @assignment.questions
    @num_questions = @assignment.flattened_questions.count
    @answers = YAML.load(File.open(@submission.upload.submission_path))
    if current_user_site_admin? || current_user_staff_for?(@course) || @submission.grades.first.available?
      if @submission.grades.first.grading_output
        @grades = YAML.load(File.open(@submission.grades.first.grading_output))
        @grades["grader"] = User.find(@grades["grader"]).name
      else
        @grades = {}
      end
    else
      @grades = {}
    end

    @show_grades = false
    
    @related_subs = @submission.review_feedbacks.map(&:submission)
    @answers_are_newer = []
    @submission_info = @related_subs.map do |sub, answers|
      d, f = sub.get_submission_files(current_user)
      @answers_are_newer << (sub.created_at < @submission.created_at)
      [d, f, sub.id]
    end
    render "details_codereview"
  end

  protected
  def setup_matchings
    # There are a few ways that codereview matchings can be pre-specified (see matching_allocations_controller)
    # but in the general case where they *aren't* pre-specified, we need to allocate them
    # on demand and in a guaranteed-fair way:
    #  Find all the available submissions, that aren't the current @team/current_user's,
    #  group them by how many reviews they already have been allocated,
    #  then randomly select from the least-allocated ones until a sufficient count is reached.
    @assn_review_count = @assignment.review_count
    @rel_team_subs = @assignment.related_assignment.team_subs?
    CodereviewMatching.transaction do
      # Find all the submissions that have already been allocated to @team/current_user to review
      matchings =
        if @assignment.team_subs?
          matchings = CodereviewMatching.where(assignment: @assignment, team: @team)
        else
          matchings = CodereviewMatching.where(assignment: @assignment, user: current_user)
        end
      users =
        if @rel_team_subs
          Team.users_for_teams(matchings.select(:target_team_id)).select(:id).map(&:id)
        else
          matchings.map(&:target_user_id).compact
        end
      @subs_to_review = @assignment.related_assignment.used_submissions.where(user_id: users).to_a
      # If there aren't enough allocations yet,
      if @subs_to_review.count < @assn_review_count
        used_sub_ids = current_user.used_submissions_for(@assignment.related_assignment).map(&:submission_id)
        # Find all the available subs
        available = @assignment.related_assignment.used_submissions
                    .where.not(id: used_sub_ids) # excluding those for @team/current_user
        # Select all the matchings for those
        grouped = {}
        if @rel_team_subs
          CodereviewMatching
            .where(assignment: @assignment, target_team_id: available.select(:team_id))
            .group(:target_team_id).count
            .each do |target_team_id, count|
            grouped[count] ||= []
            grouped[count] << target_team_id
          end
          available = available.map{|a| [a.team_id, a]}.to_h
          grouped.each do |count, team_ids|
            grouped[count] = available.values_at(*team_ids)
            team_ids.each do |team_id| available.delete team_id end
          end
        else
          CodereviewMatching
            .where(assignment: @assignment, target_user_id: available.select(:user_id))
            .group(:target_user_id).count
            .each do |target_user_id, count|
            grouped[count] ||= []
            grouped[count] << target_user_id
          end
          available = available.map{|a| [a.user_id, a]}.to_h
          grouped.each do |count, user_ids|
            grouped[count] = available.values_at(*user_ids)
            user_ids.each do |user_id| available.delete user_id end
          end
        end
        grouped[0] = available.values
        # Starting from assignments with zero reviews allocated for them yet,
        # look for sufficiently many to specify for the current_user/@team
        grouped.keys.sort.each do |group_count|
          break if @subs_to_review.count >= @assn_review_count
          new_subs = grouped[group_count]
          if new_subs.count > (@assn_review_count - @subs_to_review.count)
            new_subs = new_subs.to_a.shuffle!.take(@assn_review_count - @subs_to_review.count)
          end
          # Reserve the matchings for the new subs, so reviews stay evenly distributed
          new_subs.each do |ns|
            if @assignment.team_subs?
              if @rel_team_subs
                CodereviewMatching.create!(assignment: @assignment, team: @team, target_team_id: ns.team_id)
              else
                CodereviewMatching.create!(assignment: @assignment, team: @team, target_user_id: ns.user_id)
              end
            else
              if @rel_team_subs
                CodereviewMatching.create!(assignment: @assignment, user: current_user, target_team_id: ns.team_id)
              else
                CodereviewMatching.create!(assignment: @assignment, user: current_user, target_user_id: ns.user_id)
              end
            end
          end
          @subs_to_review += new_subs
        end
      end
    end
  rescue ActiveRecord::RecordInvalid => exception
    puts exception
    raise exception
  end
end
