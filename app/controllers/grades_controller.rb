require 'tap_parser'
require 'audit'

class GradesController < ApplicationController
  before_action :find_course
  before_action :find_assignment
  before_action :find_submission, except: [:bulk_edit, :bulk_update, :bulk_edit_curve, :bulk_update_curve]
  before_action :find_grade, except: [:bulk_edit, :bulk_update, :bulk_edit_curve, :bulk_update_curve]
  before_action :find_grader, only: [:bulk_edit, :bulk_update, :bulk_edit_curve, :bulk_update_curve]
  before_action :require_current_user
  before_action -> {
    require_admin_or_staff(@submission ? course_assignment_submission_path(@course, @assignment, @submission)
                           : course_assignment_path(@course, @assignment))
  }, except: [:show, :update, :details, :bulk_edit_curve, :bulk_update_curve]
  before_action -> {
    require_admin_or_assistant(@submission ? course_assignment_submission_path(@course, @assignment, @submission)
                               : course_assignment_path(@course, @assignment))
  }, only: [:bulk_edit_curve, :bulk_update_curve]
  def edit
    if @grade.grader.autograde?
      redirect_back fallback_location: course_assignment_submission_path(@course, @assignment, @submission),
                    alert: "That grader is automatic; there is nothing to edit"
      return
    end

    self.send("edit_#{@grade.grader.type}")
  end

  def show
    if !(current_user_site_admin? || current_user_staff_for?(@course)) && !@grade.available
      redirect_back fallback_location: course_assignment_submission_path(@course, @assignment, @submission),
                    alert: "That grader is not yet available"
      return
    end
    self.send("show_#{@grade.grader.type}")
  end

  def regrade
    @grade.grader.grade(@assignment, @submission)
    @submission.compute_grade! if @submission.grade_complete?
    redirect_back fallback_location: course_assignment_submission_path(@course, @assignment, @submission)
  end

  def bulk_edit
    self.send("bulk_edit_#{@assignment.type.capitalize}")
  end

  def bulk_update
    self.send("bulk_update_#{@assignment.type.capitalize}")
  end

  def bulk_edit_curve
    self.send("bulk_edit_curve_#{@assignment.type.capitalize}")
  end

  def bulk_update_curve
    self.send("bulk_update_curve_#{@assignment.type.capitalize}")
  end

  def update
    if current_user_site_admin? || current_user_staff_for?(@course)
      self.send("update_#{@assignment.type.capitalize}")
    else
      respond_to do |f|
        f.json { render :json => {unauthorized: "Must be an admin or staff"} }
        f.html {
          redirect_back fallback_location: course_assignment_submission_path(@course, @assignment, @submission),
                        alert: "Must be an admin or staff."
        }
      end
    end
  end

  def details
    if !(current_user_site_admin? || current_user_staff_for?(@course)) && !@grade.available
      redirect_back fallback_location: course_assignment_submission_path(@course, @assignment, @submission),
                    alert: "That grader is not yet available"
      return
    end
    respond_to do |f|
      f.text {
        render plain: self.send("details_#{@grade.grader.type}")
      }
    end
  end

  def self.pretty_print_comments(comments)
    by_file = comments.group_by(&:upload_filename)
    ans = by_file.map do |fn, cs|
      fn.gsub(Regexp.new(".*extracted/?"), "") + ":\n" + cs.sort_by(&:line).map do |c|
        c.to_s(true, false)
      end.join("\n")
    end
    ans.join("\n==================================================\n")
  end

  protected

  def comments_params
    if params[:comments].is_a? String
      comms = JSON.parse(params[:comments])
    else
      comms = params[:comments]
    end
    comms.each do |c|
      if c["file"]
        c["file"] = Upload.full_path_for(c["file"])
      end
    end
    comms
  end
  def questions_params
    params[:grades].to_unsafe_h.map{|k, v| [k, array_from_hash(v)]}.to_h
  end
  def curve_params
    params[:grader].permit(:curveType, :gradeUnit,
                           :flatPoints, :flatMax,
                           :linearMapMin, :linearMapMax,
                           :contrastDegree, :contrastWeight)
  end

  def find_grade
    @grade = Grade.find_by(id: params[:id])
    if @grade.nil?
      redirect_to fallback_location: course_assignment_submission_path(params[:course_id],
                                                                        params[:assignment_id],
                                                                        params[:submission_id]),
                  alert: "No such grader"
      return
    elsif @submission && (@grade.submission_id != @submission.id)
      redirect_to fallback_location: course_assignment_submission_path(params[:course_id],
                                                                       params[:assignment_id],
                                                                       params[:submission_id]),
                  alert: "No such grader for that submission"
    end
    @grader = @grade.grader
  end

  def find_grader
    @grader = Grader.find_by(id: params[:id])
    if @grader.nil?
      redirect_to course_assignment_path(params[:course_id], params[:assignment_id]),
                  alert: "No such grader"
      return
    elsif @grader.assignment_id != @assignment.id
      redirect_to course_assignment_path(params[:course_id], params[:assignment_id]),
                  alert: "No such grader for that assignment"
      return
    end
  end

  def do_save_comments(cp, cp_to_comment)
    # delete the ones marked for deletion
    deletable, commentable = cp.partition{|c| c["shouldDelete"]}
    to_delete = InlineComment.where(submission_id: params[:submission_id])
                .where(id: deletable.map{|c| c["id"].to_i}.select{|i| i < (1 << 31)})
    unless current_user_prof_for?(@course)
      to_delete = to_delete.where(user: current_user)  # Only professors can delete other grader's comments
    end
    to_delete.destroy_all
    deleted = deletable.map do |c| [c["id"], "deleted"] end.to_h
    # create the others
    comments = InlineComment.transaction do
      commentable.map do |c| self.send(cp_to_comment, c) end
    end
    newdata = commentable.zip(comments).map do |c, comm|
      [c["id"],
       if comm.is_a? InlineComment then comm.id else comm end]
    end.to_h
    newdata.merge(deleted)
  end

  def autosave_comments(cp, cp_to_comment)
    render :json => do_save_comments(cp, cp_to_comment)
  end

  def save_all_comments(cp, cp_to_comment)
    do_save_comments(cp, cp_to_comment)
    @grade.grader.grade(@assignment, @submission)
    @submission.compute_grade! if @submission.grade_complete?
  end

  def mark_grading_allocation_completed
    allocs = GraderAllocation.where(
      assignment: @assignment,
      course: @course,
      submission: @submission).to_a
    # there should be at most one grading allocation per user per assignment,
    # and at most one grading allocation should not be abandoned at any given time
    # but prefer to find the one for the current user, first
    alloc = (allocs.find{|a| a.who_grades_id == current_user.id} || allocs.find{|a| !a.abandoned && a.grading_completed.nil?})
    if alloc && alloc.grading_completed.nil?
      if alloc.who_grades_id != current_user.id
        alloc.abandoned = true
        alloc.grading_completed = DateTime.now
        alloc.save
        alloc = GraderAllocation.new(
          assignment: @assignment,
          course: @course,
          submission: @submission,
          who_grades_id: current_user.id,
          grading_assigned: alloc.grading_assigned)
      end
      alloc.abandoned = false
      alloc.grading_completed = DateTime.now
      alloc.save
    elsif alloc.nil?
      GraderAllocation.create!(
        abandoned: false,
        who_grades_id: current_user.id,
        grading_assigned: @assignment.due_date,
        grading_completed: DateTime.now,
        course: @course,
        assignment: @assignment,
        submission: @submission
      )        
    end
  end

  def comment_to_inlinecomment(c)
    if c["id"]
      comment = InlineComment.find_by(id: c["id"].to_i)
    end
    if comment.nil?
      comment = InlineComment.new
    end
    if c["shouldDelete"]
      comment
    else
      begin
        comment.update!(submission_id: params[:submission_id],
                        label: c["label"],
                        filename: c["file"],
                        line: c["line"],
                        grade_id: @grade.id,
                        user_id: current_user.id,
                        severity: c["severity"],
                        comment: c["comment"],
                        weight: c["deduction"],
                        suppressed: false,
                        title: "",
                        info: c["info"])
        comment
      rescue Exception => e
        { id: c["id"], error: e }
      end
    end
  end

  def question_to_inlinecomment(c)
    comment = InlineComment.find_or_initialize_by(submission_id: params[:submission_id],
                                                  grade_id: @grade.id,
                                                  line: c["index"])
    comment.update(label: "Graded question",
                   filename: @submission.upload.submission_path,
                   severity: InlineComment.severities["info"],
                   user_id: current_user.id,
                   weight: c["score"],
                   comment: c["comment"],
                   suppressed: false,
                   title: "",
                   info: nil)
    comment
  end

  ###################################
  # Per-assignment-type actions, by action
  # Bulk editing of grades
  def bulk_edit_Exam
    edit_exam_grades_for(@course.students_with_drop_info)
    
    render "edit_#{@assignment.type.underscore}_grades"
  end
  def bulk_edit_Files
    redirect_back fallback_location: course_assignment_path(@course, @assignment),
                  alert: "Bulk grade editing for that assignment type is not supported"
  end
  def bulk_edit_Questions
    redirect_back fallback_location: course_assignment_path(@course, @assignment),
                  alert: "Bulk grade editing for that assignment type is not supported"
  end
  def bulk_edit_Codereview
    redirect_back fallback_location: course_assignment_path(@course, @assignment),
                  alert: "Bulk grade editing for that assignment type is not supported"
  end
  
  def bulk_edit_curve_Files
    redirect_back fallback_location: course_assignment_path(@course, @assignment),
                  alert: "Bulk curved grade editing for that assignment type is not supported"
  end
  def bulk_edit_curve_Questions
    redirect_back fallback_location: course_assignment_path(@course, @assignment),
                  alert: "Bulk curved grade editing for that assignment type is not supported"
  end
  def bulk_edit_curve_Codereview
    redirect_back fallback_location: course_assignment_path(@course, @assignment),
                  alert: "Bulk curved grade editing for that assignment type is not supported"
  end
  
  # Bulk updates of grades
  def bulk_update_Exam
    respond_to do |f|
      f.json {
        case params[:grade_action]
        when "getGrades"
          sub = @assignment.used_sub_for(User.find(params[:user_id]))
          if sub
            comments = InlineComment.where(submission_id: sub.id)
            render :json => {grades: (comments.to_a.map do |c| [c.line, c.weight] end.to_h),
                             timestamp: Time.now}
          else
            render :json => {"none found": true}
          end
        when "setGrades"
          sub = @assignment.used_sub_for(User.find(params[:user_id]))
          config = Grade.find_by(grader_id: @grader.id, submission_id: sub.id) if sub
          if (config && (config.updated_at > Time.parse(params[:timestamp])))
            render :json => {existingTimestamp: config.updated_at, yourTimestamp: params[:timestamp]},
                   :status => 409 # conflicting request
          else
            config.save! if config
            update_exam_grades
            render :json => params
          end
        else
          render :json => params, :status => 400
        end
      }
    end
  end

  def bulk_edit_curve_Exam
    edit_exam_grades_for(@course.students_with_drop_info)
    
    render "edit_#{@assignment.type.underscore}_curved_grades"
  end
  def bulk_update_curve_Exam
    unless ["Points", "Percent"].member?(curve_params[:gradeUnit])
      redirect_back fallback_location: course_assignment_path(@course, @assignment),
                    alert: "Unknown grade unit #{curve_params[:gradeUnit]} for curve"
      return
    end
    unless ["flat", "linear", "contrast", "clear"].member?(curve_params[:curveType])
      redirect_back fallback_location: course_assignment_path(@course, @assignment),
                    alert: "Unknown curve type #{curve_params[:curveType]}"
      return
    end
    puts "Curving assignment #{@assignment.id} (course #{@course.id}): user #{current_user.name} (id #{current_user.id}), params #{curve_params}"
    total = @assignment.graders.first.avail_score
    newCurveCount = 0
    existingCurveCount = 0
    prompt = "curved"
    case curve_params[:curveType]
    when "flat"
      added = curve_params[:flatPoints].to_f
      cap = curve_params[:flatMax].to_f
      case curve_params[:gradeUnit]
      when "Points"
        @assignment.submissions.each do |sub|
          sub.set_curved_grade(current_user) do |num_questions, grades, curved|
            if curved
              existingCurveCount += 1
              false
            else
              newCurveCount += 1
              score = grades.sum{|c| c[:score]}
              [cap, score + added].min
            end
          end
        end
      when "Percent"
        @assignment.submissions.each do |sub|
          sub.set_curved_grade(current_user) do |num_questions, grades, curved|
            if curved
              existingCurveCount += 1
              false
            else
              newCurveCount += 1
              score = grades.sum{|c| c[:score]}
              [total * (cap / 100.0), score + (added / 100.0) * total].min
            end
          end
        end
      end
    when "linear"
      minCurved = curve_params[:linearMapMin].to_f
      maxCurved = curve_params[:linearMapMax].to_f
      case curve_params[:gradeUnit]
      when "Points"
        slope = (maxCurved - minCurved) / total
        intercept = minCurved
      when "Percent"
        slope = (maxCurved - minCurved) / 100.0
        intercept = minCurved * (total / 100.0)
      end
      @assignment.submissions.each do |sub|
        sub.set_curved_grade(current_user) do |num_questions, grades, curved|
          if curved
            existingCurveCount += 1
            false
          else
            newCurveCount += 1
            score = grades.sum{|c| c[:score]}
            slope * score + intercept
          end
        end
      end
    when "contrast"
      degree = curve_params[:contrastDegree].to_f
      weight = curve_params[:contrastWeight].to_f / 100.0
      @assignment.submissions.each do |sub|
        sub.set_curved_grade(current_user) do |num_questions, grades, curved|
          if curved
            existingCurveCount += 1
            false
          else
            newCurveCount += 1
            score = grades.sum{|c| c[:score]}
            scorePct = (score / total)
            contrast = scorePct ** degree
            curved = (contrast * weight) + ((1 - weight) * scorePct)
            curved * total
          end
        end
      end
    when "clear"
      prompt = "uncurved"
      @assignment.submissions.each do |sub|
        newCurveCount += 1
        sub.set_curved_grade(current_user, nil)
      end
    end
    notice = "#{pluralize(newCurveCount, 'exam grade')} #{prompt}"
    if existingCurveCount > 0
      notice += " (#{pluralize(existingCurveCount, 'exam grade')} already curved)"
    end
    redirect_to course_assignment_path(@course, @assignment), notice: notice
  end
  
  def bulk_update_Files
    redirect_back fallback_location: course_assignment_path(@course, @assignment),
                  alert: "Bulk grade updating for that assignment type is not supported"
  end
  def bulk_update_Questions
    redirect_back fallback_location: course_assignment_path(@course, @assignment),
                  alert: "Bulk grade updating for that assignment type is not supported"
  end
  def bulk_update_Codereview
    redirect_back fallback_location: course_assignment_path(@course, @assignment),
                  alert: "Bulk grade updating for that assignment type is not supported"
  end
  def bulk_update_curve_Files
    redirect_back fallback_location: course_assignment_path(@course, @assignment),
                  alert: "Bulk grade updating for that assignment type is not supported"
  end
  def bulk_update_curve_Questions
    redirect_back fallback_location: course_assignment_path(@course, @assignment),
                  alert: "Bulk grade updating for that assignment type is not supported"
  end
  def bulk_update_curve_Codereview
    redirect_back fallback_location: course_assignment_path(@course, @assignment),
                  alert: "Bulk grade updating for that assignment type is not supported"
  end

  # Individual updates, mostly of comments
  def update_Files
    respond_to do |f|
      f.json { autosave_comments(comments_params, :comment_to_inlinecomment) }
      f.html {
        save_all_comments(comments_params, :comment_to_inlinecomment)
        mark_grading_allocation_completed
        redirect_to course_assignment_submission_path(@course, @assignment, @submission),
                    notice: "Comments saved; grading completed"
      }
    end
  end
  def update_Questions
    missing, qp = questions_params[@submission.id.to_s].partition{|q| q["score"].nil? || q["score"].empty? }
    missing = missing.map{|q| q["index"].to_i + 1}

    save_all_comments(qp, :question_to_inlinecomment)
    if missing.empty?
      mark_grading_allocation_completed
      redirect_to course_assignment_submission_path(@course, @assignment, @submission),
                  notice: "Comments saved; grading completed"
    else
      if missing.count > 1
        @grade.errors.add(:base, "Questions #{missing.to_sentence} do not have grades")
      else
        @grade.errors.add(:base, "Question #{missing[0]} does not have a grade")
      end
      setup_QuestionsGrader
      @grades = [["grader", @current_user.name],
                 [@submission.id.to_s,
                  questions_params[@submission.id.to_s].map{|q| [q.delete("index").to_i, q]}.to_h]].to_h
      @show_grades = true
      render "edit_QuestionsGrader"
      return
    end
  end
  def update_Codereview
    flat_questions = @assignment.flattened_questions
    qcount = flat_questions.count
    total = flat_questions.reduce(0){|sum, q| sum + q["weight"].to_f}
    flat_responses = questions_params.map.with_index.map do |(k, v), i|
      v.each do |q|
        q["index"] = q["index"].to_i + i * qcount
      end
      v
    end.flatten
    missing, qp = flat_responses.partition{|q| q["score"].nil? || q["score"].empty? }
    missing = missing.map{|q| q["index"].to_i + 1}

    if !missing.empty?
      if missing.count > 1
        @grade.errors.add(:base, "Questions #{missing.to_sentence} do not have grades")
      else
        @grade.errors.add(:base, "Question #{missing[0]} does not have a grade")
      end
      setup_CodereviewGrader
      @grades = questions_params
      render "edit_CodereviewGrader"
      return
    end

    all_feedbacks = @submission.review_feedbacks.to_a
    questions_params.map do |k, v|
      feedback = all_feedbacks.find{|f| f.submission_id == k.to_i}
      if feedback.nil?
        redirect_to :back, alert: "Submitted feedback for an unexpected submission #{k}"
      else
        feedback.grade_id = @grade.id
        feedback.score, feedback.out_of = @grade.grader.partial_grade_for_sub(@assignment, @grade, k)
        feedback.save
      end
    end

    grader_dir = @submission.upload.grader_path(@grade)
    grader_dir.mkpath
    File.open(grader_dir.join("grades.yaml"), "w") do |grades|
      to_write = questions_params
      to_write["grader"] = current_user.id
      grades.write(YAML.dump(to_write))
      @grade.grading_output = grades.path
      @grade.save
    end

    @grade.grader.grade(@assignment, @submission)
    @submission.compute_grade! if @submission.grade_complete?
    mark_grading_allocation_completed
    redirect_to course_assignment_submission_path(@course, @assignment, @submission),
                notice: "Comments saved; grading completed"
  end
  def update_Exam
    update_exam_grades
    redirect_to course_assignment_submission_path(@course, @assignment, @submission),
                notice: "Grades saved"
  end

  ###################################
  # Grader responses, by grader type

  def show_inline_comment_grader
    @submission_dirs, @submission_files = @submission.get_submission_files(current_user, nil, @grade.id)
    @commentsFrom = @grade.id
    if @grade.grading_output
      begin
        @grading_output = File.read(@grade.grading_output)
        begin
          tap = TapParser.new(@grading_output)
          @grading_output = tap
          @tests = tap.tests
        rescue Exception
          @tests = []
        end
      rescue Errno::ENOENT
        @grading_output = "Grading output file is missing or could not be read"
        @tests = []
      end
    end
    num_comments = @grade.inline_comments.where.not(line: 0).count
    cur_reg = current_user.registration_for(@course)
    @sub_comments = @submission.grade_submission_comments(current_user.site_admin? || cur_reg.staff?)
    if @sub_comments.empty? && (@tests.nil? || @tests.count != num_comments)
      @error_header = <<HEADER.html_safe
<p>There seems to be a problem displaying the #{@grade.grader.type.humanize}'s feedback on this submission.</p>
<p>Please email the professor, with the following information:</p>
<ul>
<li>Course: #{@course.id}</li>
<li>Assignment: #{@assignment.id}</li>
<li>Submission: #{@submission.id}</li>
<li>Grader: #{@grade.id}</li>
<li>User: #{current_user.name} (#{current_user.id})</li>
<li>Reason: #{if @sub_comments.empty? then "No comments" else "Wrong test count" end}</li>
</li>
HEADER
    end
    render "submissions/details_files"
  end
  
  # JavaStyleGrader
  def show_JavaStyleGrader
    show_inline_comment_grader
  end
  def edit_JavaStyleGrader
    redirect_to details_course_assignment_submission_path(@course, @assignment, @submission)
  end
  def details_JavaStyleGrader
    GradesController.pretty_print_comments(@grade.inline_comments)
  end

  # JunitGrader
  def show_JunitGrader
    if @grade.grading_output
      begin
        @grading_output = File.read(@grade.grading_output)
        begin
          tap = TapParser.new(@grading_output)
          @grading_output = tap
          @tests = tap.tests
        rescue Exception
          @tests = []
        end
      rescue Errno::ENOENT
        @grading_output = "Grading output file is missing or could not be read"
        @tests = []
      end
    end

    if current_user_site_admin? || current_user_staff_for?(@course)
      if @grading_output.kind_of?(String)
        @grading_header = "Errors running tests"
      else
        @grading_header = "All test results"
        @tests = @grading_output.tests
      end
    else
      if @grading_output.kind_of?(String)
        @grading_header = "Errors running tests"
      elsif @grading_output.passed_count == @grading_output.test_count
        @grading_header = "Test results"
        @tests = @grading_output.tests
      else
        @grading_header = "Selected test results"
        @tests = @grading_output.tests.reject{|t| t[:passed]}.shuffle!.take(@grade.grader.errors_to_show || 3)
      end
    end

    render "show_JunitGrader"
  end
  def details_JunitGrader
    "No details to show for Junit grader"
  end

  # CheckerGrader
  def show_CheckerGrader
    if @grade.grading_output
      begin
        @grading_output = File.read(@grade.grading_output)
        begin
          tap = TapParser.new(@grading_output)
          @grading_output = tap
          @tests = tap.tests
        rescue Exception
          @tests = []
        end
      rescue Errno::ENOENT
        @grading_output = "Grading output file is missing or could not be read"
        @tests = []
      end
    end

    if current_user_site_admin? || current_user_staff_for?(@course)
      if @grading_output.nil? || @grading_output.kind_of?(String)
        @grading_header = "Errors running tests"
      else
        @grading_header = "All test results"
        @tests = @grading_output.tests
      end
    else
      if @grading_output.nil? || @grading_output.kind_of?(String)
        @grading_header = "Errors running tests"
      elsif @grading_output.passed_count == @grading_output.test_count
        @grading_header = "All tests passed"
      else
        @grading_header = "Selected test results"
        @tests = @grading_output.tests.reject{|t| t[:passed]}.shuffle!.take(@grade.grader.errors_to_show || 3)
      end
    end

    render "show_CheckerGrader"
  end
  def details_CheckerGrader
    GradesController.pretty_print_comments(@grade.inline_comments)
  end

  # RacketStyleGrader
  def show_RacketStyleGrader
    show_inline_comment_grader
  end
  def edit_RacketStyleGrader
    redirect_to details_course_assignment_submission_path(@course, @assignment, @submission)
  end
  def details_RacketStyleGrader
    GradesController.pretty_print_comments(@grade.inline_comments)
  end

  # QuestionsGrader
  def edit_QuestionsGrader
    setup_QuestionsGrader
    @show_grades = true
    render "edit_QuestionsGrader"
  end
  def setup_QuestionsGrader
    @questions = @assignment.questions
    @answers = YAML.load(File.open(@submission.upload.submission_path))
    @answers = [[@submission.id.to_s, @answers]].to_h

    if @assignment.related_assignment
      related_sub = @assignment.related_assignment.used_sub_for(@submission.user)
      if related_sub.nil?
        @submission_files = []
        @submission_dirs = []
        @answers_are_newer = true
      else
        @submission_dirs, @submission_files = related_sub.get_submission_files(current_user)
        @answers_are_newer = (related_sub.created_at < @submission.created_at)
      end
    else
      @submission_files = []
      @submission_dirs = []
      @answers_are_newer = true
    end
    show_hidden = (current_user_site_admin? || current_user_staff_for?(@course))
    @grades = @submission.inline_comments
    @grades = @grades.select(:line, :name, :weight, :comment, :user_id).joins(:user).sort_by(&:line).to_a
    @grades = [["grader", (@grades&.first&.user&.name || @current_user.name)],
               [@submission.id.to_s,
                @grades.map{|g| [g.line, [["score", g.weight], ["comment", g.comment]].to_h]}.to_h]].to_h
  end
  def show_QuestionsGrader
    redirect_to details_course_assignment_submission_path(@course, @assignment, @submission)
  end
  def details_QuestionsGrader
    "No details to show for Questions grader"
  end

  # CodereviewGrader
  def edit_CodereviewGrader
    setup_CodereviewGrader
    render "edit_CodereviewGrader"
  end
  def setup_CodereviewGrader
    @questions = @assignment.questions
    @num_questions = @assignment.flattened_questions.count
    @answers = YAML.load(File.open(@submission.upload.submission_path))

    @related_subs = @submission.review_feedbacks.map(&:submission)
    @answers_are_newer = []
    @submission_info = @related_subs.map do |sub, answers|
      d, f = sub.get_submission_files(current_user)
      @answers_are_newer << (sub.created_at < @submission.created_at)
      [d, f, sub.id, sub.team&.to_s, sub.user.display_name]
    end

    if @grade.grading_output
      @grades = YAML.load(File.open(@grade.grading_output))
      @grades["grader"] = User.find(@grades["grader"]).name
    else
      @grades = {}
    end
    @show_grades = true
  end
  def show_CodereviewGrader
    redirect_to details_course_assignment_submission_path(@course, @assignment, @submission)
  end
  def details_CodereviewGrader
    "No details to show for Codereview grader"
  end

  # ManualGrader
  def edit_ManualGrader
    show_hidden = (current_user_site_admin? || current_user_staff_for?(@course))
    @lineCommentsByFile = @submission.grade_line_comments(current_user, show_hidden)
    @submission_dirs, @submission_files = @submission.get_submission_files(current_user, @lineCommentsByFile)
    render "edit_ManualGrader"
  end
  def show_ManualGrader
    show_hidden = (current_user_site_admin? || current_user_staff_for?(@course))
    @lineCommentsByFile = @submission.grade_line_comments(current_user, show_hidden)
    @sub_comments = @submission.grade_submission_comments(show_hidden)
    @submission_dirs, @submission_files = @submission.get_submission_files(current_user, @lineCommentsByFile, @grade.id)
    @commentsFrom = @grade.id
    @grading_output = @grade
    render "submissions/details_files"
  end
  def details_ManualGrader
    show_hidden = (current_user_site_admin? || current_user_staff_for?(@course))
    if show_hidden || @grade.available?
      GradesController.pretty_print_comments(@grade.inline_comments)
    else
      GradesController.pretty_print_comments([])
    end
  end

  # ExamGrader
  def edit_ExamGrader
    edit_exam_grades_for(@course.users_with_drop_info([@submission.user]))
    render "edit_ExamGrader"
  end
  def show_ExamGrader
    redirect_to course_assignment_submission_path(@course, @assignment, @submission)
  end
  def details_ExamGrader
    "No details to show for Exam grader"
  end

  def edit_SandboxGrader
    redirect_to details_course_assignment_submission_path(@course, @assignment, @submission)
  end
  def show_SandboxGrader
    if @grade.grading_output
      begin
        @grading_output = File.read(@grade.grading_output)
        begin
          tap = TapParser.new(@grading_output)
          @grading_output = tap
          @tests = tap.tests
        rescue Exception
          @tests = []
        end
      rescue Errno::ENOENT
        @grading_output = "Grading output file is missing or could not be read"
        @tests = []
      end
    end

    if current_user_site_admin? || current_user_staff_for?(@course)
      if @grading_output.kind_of?(String)
        @grading_header = "Errors running tests"
      else
        @grading_header = "All test results"
        @tests = @grading_output.tests
      end
    else
      if @grading_output.kind_of?(String)
        @grading_header = "Errors running tests"
      elsif @grading_output.passed_count == @grading_output.test_count
        @grading_header = "Test results"
        @tests = @grading_output.tests
      else
        @grading_header = "Selected test results"
        @tests = @grading_output.tests.reject{|t| t[:passed]}.shuffle!.take(3)
      end
    end

    render "show_SandboxGrader"
  end
  def details_SandboxGrader
    @grade.notes
  end


  ##############################
  def edit_exam_grades_for(students)
    # NOTE: students must be joined to the registrations table already, to provide section information
    @student_info = students.select(:username, :last_name, :first_name, :nickname, :nuid, :id)
    @sections_by_student = @course.users_with_registrations(students).select(:id, "sections.crn AS crn").group_by(&:id)
    @used_subs = @assignment.used_submissions
    @grade_comments = multi_group_by(InlineComment.where(submission_id: @used_subs.map(&:id)),
                                     [:submission_id, :line], true)
  end

  def update_exam_grades
    students_with_grades = params[:student].to_unsafe_h.reject do |k, v|
      v.values.join("") == ""
    end
    all_grades = array_from_hash(students_with_grades)
    @student_info = @course.students.select(:username, :last_name, :first_name, :id).where(id: students_with_grades.keys)
    @grader = @assignment.graders.first # and only config
    # @used_subs = @assignment.used_submissions
    # @grade_comments = InlineComment.where(submission_id: @used_subs.map(&:id)).group_by(&:user_id)

    @student_info.each do |student|
      @sub = @assignment.used_sub_for(student)
      if @sub.nil?
        @sub = Submission.create!(assignment: @assignment,
                                  user: student,
                                  type: "ExamSub",
                                  created_at: @assignment.due_date - 1.minute)
      end
      @sub.set_used_sub!
      @grade = Grade.find_or_create_by(grader_id: @grader.id, submission_id: @sub.id)
      if @grade.new_record?
        @grade.out_of = @grader.avail_score
      end
      grades = all_grades[student.id]
      flattened = @assignment.flattened_questions
      grades.each_with_index do |g, q_num|
        @sub.grade_question!(current_user, q_num, g)
      end
      @grader.expect_num_questions(flattened.count)
      @grader.grade(@assignment, @sub)
    end
  end

end
