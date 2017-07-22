class ReviewsController < CoursesController
  prepend_before_action :find_review
  prepend_before_action :find_submission
  prepend_before_action :find_course_assignment

  def show
    unless @submission.visible_to?(current_user)
      redirect_to course_assignment_path(@course, @assignment), alert: "That's not your submission"
      return
    end
    @review_submission = @review.review_submission
    @review_assignment = @review_submission.assignment
    @cur_reg = current_user.registration_for(@course)
    if @review_submission.score.nil? && !@cur_reg.staff?
      redirect_to course_assignment_submission_path(@course, @assignment, @submission),
                  alert: "This review is not yet available"
      return
    end
    
    @questions = @review_assignment.questions
    @num_questions = @review_assignment.flattened_questions.count
    @answers = YAML.load(File.open(@review_submission.upload.submission_path))

    # DO NOT show grades here -- just show the feedback
    @grades = {}
    @show_grades = false

    @related_subs = [@review.submission]
    @answers_are_newer = []
    @submission_info = @related_subs.map do |sub, answers|
      d, f = sub.get_submission_files(current_user)
      @answers_are_newer << (sub.created_at < @review_submission.created_at)
      [d, f, sub.id]
    end    
  end

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

  def find_submission
    @submission = Submission.find_by(id: params[:submission_id])
    if @submission.nil?
      redirect_back fallback_location: course_assignment_path(params[:course_id], params[:assignment_id]),
                    alert: "No such submission"
      return
    end
    if @submission.assignment_id != @assignment.id
      redirect_back fallback_location: course_assignment_path(@course, @assignment), alert: "No such submission for this assignment"
      return
    end
  end

  def find_review
    @review = ReviewFeedback.find_by(id: params[:id])
    if @review.nil? || @review.submission_id != @submission.id
      redirect_back fallback_location: course_assignment_submission_path(params[:course_id], params[:assignment_id], params[:submission_id]),
                    alert: "No such review"
      return
    end
  end
end
