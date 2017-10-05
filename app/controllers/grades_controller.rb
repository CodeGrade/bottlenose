require 'tap_parser'

class GradesController < ApplicationController
  before_action :find_course
  before_action :find_assignment
  before_action :find_submission, except: [:bulk_edit, :bulk_update]
  before_action :find_grade
  before_action -> {
    require_admin_or_staff(@submission ? course_assignment_submission_path(@course, @assignment, @submission)
                           : course_assignment_path(@course, @assignment))
  }, except: [:show, :update, :details]
  before_action :require_current_user
  def edit
    if @grade.grader.autograde?
      redirect_back fallback_location: course_assignment_submission_path(@course, @assignment, @submission),
                    alert: "That grader is automatic; there is nothing to edit"
      return
    end

    self.send("edit_#{@grade.grader.type}")
  end

  def show
    if !(current_user_site_admin? || current_user_staff_for?(@course)) and !@grade.available
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
    if !(current_user_site_admin? || current_user_staff_for?(@course)) and !@grade.available
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

  def find_grade
    @grade = Grade.find_by(id: params[:id])
    if @grade.nil?
      redirect_to fallback_location: course_assignment_submission_path(params[:course_id],
                                                                        params[:assignment_id],
                                                                        params[:submission_id]),
                  alert: "No such grader"
      return
    elsif @submission and @grade.submission_id != @submission.id
      redirect_to fallback_location: course_assignment_submission_path(params[:course_id],
                                                                       params[:assignment_id],
                                                                       params[:submission_id]),
                  alert: "No such grader for that submission"
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
    newdata = commentable.zip(comments).map do |c, comm| [c["id"], comm.id] end.to_h
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
    alloc = GraderAllocation.find_by(
      assignment: @assignment,
      course: @course,
      submission: @submission)
    if alloc and alloc.grading_completed.nil?
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
      comment.update(submission_id: params[:submission_id],
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
                     info: nil)
      comment
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
    edit_exam_grades_for(@course.students)
    
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

  # Bulk updates of grades
  def bulk_update_Exam
    respond_to do |f|
      f.json {
        if params[:grade_action] == "getGrades"
          sub = @assignment.used_sub_for(User.find(params[:user_id]))
          if sub
            comments = InlineComment.where(submission_id: sub.id)
            render :json => {grades: (comments.to_a.map do |c| [c.line, c.weight] end.to_h),
                             timestamp: Time.now}
          else
            render :json => {"none found": true}
          end
        elsif params[:grade_action] == "setGrades"
          sub = @assignment.used_sub_for(User.find(params[:user_id]))
          @grader = @assignment.graders.first # and only config
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
    missing, qp = questions_params[@submission.id.to_s].partition{|q| q["score"].nil? or q["score"].empty? }
    missing = missing.map{|q| q["index"].to_i + 1}

    save_all_comments(qp, :question_to_inlinecomment)
    if missing.empty?
      mark_grading_allocation_completed
      redirect_to course_assignment_submission_path(@course, @assignment, @submission),
                  notice: "Comments saved; grading completed"
    else
      if missing.count > 1
        @grade.errors.add(:base, "Questions #{missing.join(', ')} do not have grades")
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
    missing, qp = flat_responses.partition{|q| q["score"].nil? or q["score"].empty? }
    missing = missing.map{|q| q["index"].to_i + 1}

    if !missing.empty?
      if missing.count > 1
        @grade.errors.add(:base, "Questions #{missing.join(', ')} do not have grades")
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

  def show_inline_comment_grader(type)
    @submission_dirs, @submission_files = @submission.get_submission_files(current_user, nil, type)
    @commentType = type
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
<p>There seems to be a problem displaying the #{type.camelcase.underscore.humanize}'s feedback on this submission.</p>
<p>Please email the professor, with the following information:</p>
<ul>
<li>Course: #{@course.id}</li>
<li>Assignment: #{@assignment.id}</li>
<li>Submission: #{@submission.id}</li>
<li>Grader: #{@grade.id}</li>
<li>User: #{current_user.name} (#{current_user.id})</li>
</li>
HEADER
    end
    render "submissions/details_files"
  end
  
  # JavaStyleGrader
  def show_JavaStyleGrader
    show_inline_comment_grader "JavaStyleGrader"
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
    show_inline_comment_grader "RacketStyleGrader"
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
      [d, f, sub.id]
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
    @submission_dirs, @submission_files = @submission.get_submission_files(current_user, @lineCommentsByFile, "ManualGrader")
    @commentType = "ManualGrader"
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
    edit_exam_grades_for(User
                          .where(id: @submission.user_id)
                          .joins(:registrations)
                          .where("registrations.course_id": @course.id))
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
  def details_SanboxGrader
    @grade.notes
  end


  ##############################
  def edit_exam_grades_for(students)
    # NOTE: students must be joined to the registrations table already, to provide section information
    @student_info = students.select(:username, :last_name, :first_name, :nickname, :id)
    @students_by_section = @course.students_with_registrations.select(:id, "registration_sections.section_id AS crn").group_by(&:id)
    @used_subs = @assignment.used_submissions
    @grade_comments = InlineComment.where(submission_id: @used_subs.map(&:id)).group_by(&:submission_id)
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
        comment = InlineComment.find_or_initialize_by(submission_id: @sub.id, grade_id: @grade.id, line: q_num)
        if g.to_s.empty?
          if comment.new_record?
            next # no need to save blanks
          else
            comment.delete
          end
        else
          comment.update(label: "Exam question",
                         filename: @assignment.name,
                         severity: InlineComment.severities["info"],
                         user_id: current_user.id,
                         weight: g,
                         comment: "",
                         suppressed: false,
                         title: "",
                         info: nil)
        end
      end
      @grader.expect_num_questions(flattened.count)
      @grader.grade(@assignment, @sub)
    end
  end

end
