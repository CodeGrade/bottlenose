require 'tempfile'
require 'audit'

class SubmissionsController < CoursesController
  prepend_before_action :find_submission, except: [:index, :new, :create, :rerun_grader]
  prepend_before_action :find_course_assignment
  before_action :require_current_user, only: [:show, :files, :new, :create]
  before_action :require_admin_or_staff, only: [:recreate_grader, :rerun_grader, :use_for_grading, :publish]
  before_action :require_admin_or_prof, only: [:rescind_lateness, :edit_plagiarism, :update_plagiarism]
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
    @submission = Submission.new
    @submission.assignment_id = @assignment.id
    @submission.user_id = current_user.id

    if @assignment.team_subs?
      @team = current_user.active_team_for(@course)

      if @team.nil? && current_user.course_staff?(@course)
        @team = Team.new(course: @course, start_date: DateTime.now)
        @team.users = [current_user]
        @team.save
      end

      @submission.team = @team
    end

    self.send("new_#{@assignment.type.capitalize}")
  end

  def create
    @submission = Submission.new(submission_params)
    @submission.assignment_id = @assignment.id
    if @assignment.team_subs?
      @team = current_user.active_team_for(@course)
      @submission.team = @team
    end

    @row_user = User.find_by_id(params[:row_user_id])

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

  def recreate_grader
    @grader_config = GraderConfig.find(params[:grader_config_id])
    if @submission.recreate_missing_grader(@grader_config)
      @submission.compute_grade! if @submission.grade_complete?
      redirect_to back_or_else(course_assignment_submission_path(@course, @assignment, @submission))
    else
      redirect_to back_or_else(course_assignment_submission_path(@course, @assignment, @submission)),
                  alert: "Grader already exists; use Regrade to modify it instead"
    end
  end

  def rerun_grader
    @grader_config = GraderConfig.find(params[:grader_config_id])
    count = 0
    @assignment.used_submissions.each do |sub|
      @grader_config.delay.grade(@assignment, sub)
      count += 1
    end
    redirect_to back_or_else(course_assignment_path(@course, @assignment)),
                notice: "Regraded #{@grader_config.display_type} for #{plural(count, 'submission')}"
  end

  def use_for_grading
    @submission.set_used_sub!
    redirect_to back_or_else(course_assignment_submission_path(@course, @assignment, @submission))
  end

  def rescind_lateness
    @submission.update_attribute(:ignore_late_penalty, true)
    @submission.compute_grade!
    redirect_to back_or_else(course_assignment_submission_path(@course, @assignment, @submission))
  end

  def edit_plagiarism
    @max_points = @assignment.grader_configs.map(&:avail_score).sum
  end

  def update_plagiarism
    guilty_students = params[:submission] || {}
    guilty_students = guilty_students[:penalty] || {}
    guilty_students = guilty_students.map{|k, v| [k.to_i, v.to_f]}.to_h
    if guilty_students.empty?
      redirect_to back_or_else(edit_plagiarism_course_assignment_submission_path(@course, @assignment, @submission)),
                  alert: "You haven't selected any students as being involved"
      return
    end
    comment = params[:comment]
    if comment.to_s.empty?
      redirect_to back_or_else(edit_plagiarism_course_assignment_submission_path(@course, @assignment, @submission)),
                  alert: "Must leave some explanatory comment for the students"
      return
    end

    @max_points = @assignment.grader_configs.map(&:avail_score).sum
    guilty_students.each do |id, penalty|
      penaltyPct = (100.0 * penalty.to_f) / @max_score.to_f
      student = User.find(id)
      team = @submission.team
      if team
        # Construct a one-use team, so that this student can be penalized in isolation
        team = Team.new(
          course: @course,
          start_date: @submission.created_at,
          end_date: @submission.created_at)
        team.users = [student]
        team.save
      end
      # Create the new submission, reusing the prior submitted file
      sub = Submission.new(
        assignment: @assignment,
        user: student,
        time_taken: @submission.time_taken,
        created_at: @submission.created_at,
        ignore_late_penalty: false,
        score: [@submission.score.to_f - penaltyPct, 0].max,
        team: team,
        upload_id: @submission.upload_id,
        type: @submission.type)
      sub.save
      sub.set_used_sub!
      # Add the penalty
      sub_comment = InlineComment.new(
        submission: sub,
        label: "Plagiarism",
        line: 0,
        filename: sub.upload.extracted_path,
        grader_id: current_user.id,
        severity: "error",
        comment: comment,
        weight: penalty,
        suppressed: false,
        title: "",
        info: nil)
      sub_comment.save
      # Copy any graders
      @submission.graders.each do |g|
        new_g = Grader.new(
          grader_config_id: g.grader_config_id,
          submission_id: sub.id,
          grading_output: g.grading_output,
          score: g.score,
          out_of: g.out_of,
          available: g.available)
        new_g.save
      end
    end
    redirect_to course_assignment_path(@course, @assignment)
  end

  def publish
    @submission.graders.where(score: nil).each do |g| g.grade(assignment, used) end
    @submission.graders.update_all(:available => true)
    @submission.compute_grade!
    redirect_to back_or_else(course_assignment_submission_path(@course, @assignment, @submission))
  end


  private

  def submission_params
    if true_user_prof_for?(@course) or current_user_prof_for?(@course)
      params[:submission].permit(:assignment_id, :user_id, :student_notes,
                                 :auto_score, :calc_score, :created_at, :updated_at, :upload,
                                 :grading_output, :grading_uid, :team_id,
                                 :teacher_score, :teacher_notes,
                                 :ignore_late_penalty, :upload_file,
                                 :comments_upload_file, :time_taken)
    else
      params[:submission].permit(:assignment_id, :user_id, :student_notes,
                                 :upload, :upload_file, :time_taken)
    end
  end

  def answers_params
    array_from_hash(params[:answers])
  end

  def require_admin_or_staff
    unless current_user_site_admin? || current_user_staff_for?(@course)
      redirect_to back_or_else(course_assignment_submission_path(@course, @assignment, @submission)),
                  alert: "Must be an admin or staff."
      return
    end
  end

  def require_admin_or_prof
    unless current_user_site_admin? || current_user_prof_for?(@course)
      redirect_to back_or_else(course_assignment_submission_path(@course, @assignment, @submission)),
                  alert: "Must be an admin or professor."
      return
    end
  end

  def find_course_assignment
    @course = Course.find_by(id: params[:course_id])
    @assignment = Assignment.find_by(id: params[:assignment_id])
    if @course.nil?
      redirect_to back_or_else(root_path), alert: "No such course"
      return
    end
    if @assignment.nil? or @assignment.course_id != @course.id
      redirect_to back_or_else(course_path(@course)), alert: "No such assignment for this course"
      return
    end
  end

  def find_submission
    @submission = Submission.find_by(id: params[:id])
    if @submission.nil?
      redirect_to back_or_else(course_assignment_path(params[:course_id], params[:assignment_id])),
                  alert: "No such submission"
      return
    end
    if @submission.assignment_id != @assignment.id
      redirect_to back_or_else(course_assignment_path(@course, @assignment)), alert: "No such submission for this assignment"
      return
    end
  end


  ######################
  # Assignment types
  # NEW
  def new_Files
    render "new_#{@assignment.type.underscore}"
  end

  def new_Questions
    @questions = @assignment.questions
    @submission_dirs = []
    if @assignment.related_assignment
      related_sub = @assignment.related_assignment.used_sub_for(current_user)
      Audit.log("User #{current_user.id} (#{current_user.name}) is viewing the self-eval for assignment #{@assignment.related_assignment.id} and has agreed not to submit further files to it.\n")
      if related_sub.nil?
        @submission_files = []
      else
        get_submission_files(related_sub)
      end
    else
      @submission_files = []
    end
    render "new_#{@assignment.type.underscore}"
  end

  def new_Exam
    unless current_user_site_admin? || current_user_staff_for?(@course)
      redirect_to back_or_else(course_assignment_path(@course, @assignment)),
                  alert: "Must be an admin or staff to enter exam grades."
      return
    end
    @grader_config = @assignment.grader_configs.first
    redirect_to bulk_course_assignment_grader_config_path(@course, @assignment, @grader_config)
  end

  # CREATE
  def create_Files
    no_problems = true
    time_taken = submission_params[:time_taken]
    if @assignment.request_time_taken and time_taken.empty?
      @submission.errors[:base] << "Please specify how long you have worked on this assignment"
      no_problems = false
    elsif time_taken and !(Float(time_taken) rescue false)
      @submission.errors[:base] << "Please specify a valid number for how long you have worked on this assignment"
      no_problems = false
    end
    if submission_params[:upload_file].nil?
      @submission.errors[:base] << "You need to submit a file."
      no_problems = false
    end
    no_problems = (no_problems and @submission.save_upload and @submission.save)
    if no_problems
      @submission.set_used_sub!
      @submission.create_graders!
      @submission.delay.autograde!
      path = course_assignment_submission_path(@course, @assignment, @submission)
      redirect_to(path, notice: 'Submission was successfully created.')
    else
      @submission.cleanup!
      new_Files
    end
  end

  def create_Questions
    @answers = answers_params
    questions = @assignment.questions.reduce([]) do |acc, section|
      section.reduce(acc) do |acc, (name, qs)| acc + qs end
    end
    num_qs = questions.count
    no_problems = true
    time_taken = submission_params[:time_taken]
    if @assignment.request_time_taken and time_taken.empty?
      @submission.errors[:base] << "Please specify how long you have worked on this assignment"
      no_problems = false
    elsif time_taken and !(Float(time_taken) rescue false)
      @submission.errors[:base] << "Please specify a valid number for how long you have worked on this assignment"
      no_problems = false
    end
    if @answers.count != num_qs
      @submission.errors.add(:base, "There were #{plural(@answers.count, 'answer')} for #{plural(num_qs, 'question')}")
      @submission.cleanup!
      no_problems = false
    else
      questions.zip(@answers).each_with_index do |(q, a), i|
        if a.nil? or a["main"].nil?
          @submission.errors.add(:base, "Question #{i + 1} is missing an answer")
          no_problems = false
          next
        end
        if q["YesNo"]
          type = "YesNo"
          unless ["yes", "no"].member?(a["main"].downcase)
            @submission.errors.add(:base, "Question #{i + 1} has a non-Yes/No answer")
            no_problems = false
          end
        elsif q["TrueFalse"]
          type = "TrueFalse"
          unless ["true", "false"].member?(a["main"].downcase)
            @submission.errors.add(:base, "Question #{i + 1} has non-true/false answer")
            no_problems = false
          end
        elsif q["Numeric"]
          type = "Numeric"
          if !(Float(a["main"]) rescue false)
            @submission.errors.add(:base, "Question #{i + 1} has a non-numeric answer")
            no_problems = false
          elsif Float(a["main"]) < q["Numeric"]["min"] or Float(a["main"]) > q["Numeric"]["max"]
            @submission.errors.add(:base, "Question #{i + 1} has a numeric answer outside the valid range")
            no_problems = false
          end
        elsif q["MultipleChoice"]
          type = "MultipleChoice"
          if a["main"].nil?
            # nothing, was handled above
          elsif !(Integer(a["main"]) rescue false)
            @submission.errors.add(:base, "Question #{i + 1} has an invalid multiple-choice answer")
            no_problems = false
          elsif a["main"].to_i < 0 or a["main"].to_i >= q[type]["options"].count
            @submission.errors.add(:base, "Question #{i + 1} has an invalid multiple-choice answer")
            no_problems = false
          end
        elsif q["Text"]
          type = "Text"
        end
        if q[type]["parts"]
          if a["parts"].nil? or q[type]["parts"].count != a["parts"].count
            @submission.errors.add(:base, "Question #{i + 1} is missing answers to its sub-parts")
            no_problems = false
          else
            q[type]["parts"].zip(a["parts"]).each_with_index do |(qp, ap), j|
              if qp["codeTag"]
                if @assignment.related_assignment
                  related_sub = @assignment.related_assignment.used_sub_for(current_user)
                  if related_sub.nil?
                    @submission_files = []
                  else
                    get_submission_files(related_sub)
                  end
                  if ap["file"].to_s == "<none>"
                  # nothing
                  elsif !(@submission_files.any?{|f| f[:link] == ap["file"].to_s}) or !(Integer(ap["line"]) rescue false)
                    @submission.errors.add(:base, "Question #{i + 1} part #{j + 1} has an invalid code-tag")
                    no_problems = false
                  end
                else
                  @submission.errors.add(:base, "Question #{i + 1} part #{j + 1} has a code-tag, but there is no submission related to these questions!  Please email your professor.")
                  no_problems = false
                end
              elsif qp["codeTags"]
                # TODO
              elsif qp["text"]
                # TODO
              elsif qp["requiredText"]
                if ap["info"].to_s.empty?
                  @submission.errors.add(:base, "Question #{i + 1} part #{j + 1} has a missing required text answer")
                  no_problems = false
                end
              end
            end
          end
        end
      end
    end

    if no_problems
      Tempfile.open('answers.yaml', Rails.root.join('tmp')) do |f|
        f.write(YAML.dump(@answers))
        f.flush
        f.rewind
        uploadfile = ActionDispatch::Http::UploadedFile.new(filename: "answers.yaml", tempfile: f)
        @submission.upload_file = uploadfile
        @submission.save_upload
      end
      @submission.type = "Questions"
      @submission.save
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
    # No graders are created here, because we shouldn't ever get to this code
    # The graders are configured in the GradersController, in update_exam_grades
  end


  # SHOW
  def show_Files
    @gradesheet = Gradesheet.new(@assignment, [@submission])
    @plagiarized = @submission.grader_submission_comments(false) || {}
    @plagiarized = @plagiarized[@submission.upload.extracted_path.to_s] || []
    @plagiarized = @plagiarized.any?{|c| c.label == "Plagiarism"}
  end
  def show_Questions
    @gradesheet = Gradesheet.new(@assignment, [@submission])
    @questions = @assignment.questions
    @answers = YAML.load(File.open(@submission.upload.submission_path))
    @submission_dirs = []
    if @assignment.related_assignment
      @related_sub = @assignment.related_assignment.used_sub_for(@submission.user)
      if @related_sub.nil?
        @submission_files = []
        @answers_are_newer = true
      else
        get_submission_files(@related_sub)
        @answers_are_newer = (@related_sub.created_at < @submission.created_at)
      end
    else
      @submission_files = []
      @answers_are_newer = true
    end
  end
  def show_Exam
    @student_info = @course.students.select(:username, :last_name, :first_name, :id)
    @grader_config = @assignment.grader_configs.first
    @grader = Grader.find_by(grader_config_id: @grader_config.id, submission_id: @submission.id)
    @grade_comments = InlineComment.where(submission_id: @submission.id).order(:line).to_a
  end

  # DETAILS
  def details_Files
    respond_to do |f|
      f.html {
        get_submission_files(@submission, nil, true)
        render "details_#{@assignment.type.underscore}"
      }
      f.text {
        show_hidden = (current_user_site_admin? || current_user_staff_for?(@course))
        comments = @submission.graders
        unless show_hidden
          comments = comments.where(available: true)
        end
        comments = comments.map(&:inline_comments).flatten
        render :text => GradersController.pretty_print_comments(comments)
      }
    end
  end
  def details_Questions
    @questions = @assignment.questions
    @answers = YAML.load(File.open(@submission.upload.submission_path))
    if current_user_site_admin? || current_user_staff_for?(@course)
      @grades = @submission.inline_comments
    else
      @grades = @submission.visible_inline_comments
    end

    @show_graders = false

    @grades = @grades.select(:line, :name, :weight, :comment).joins(:user).sort_by(&:line).to_a
    @submission_dirs = []
    if @assignment.related_assignment
      @related_sub = @assignment.related_assignment.used_sub_for(@submission.user)
      if @related_sub.nil?
        @submission_files = []
        @answers_are_newer = true
      else
        get_submission_files(@related_sub)
        @answers_are_newer = (@related_sub.created_at < @submission.created_at)
      end
    else
      @submission_files = []
      @answers_are_newer = true
    end
    render "details_questions"
  end
  def details_Exam
    render "details_exam"
  end
end
