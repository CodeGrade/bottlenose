class ReviewsController < ApplicationController
  layout 'course'
  
  before_action :require_registered_user
  before_action :find_course
  before_action :find_assignment
  before_action :find_submission
  before_action :find_review

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
      [d, f, sub.upload, sub.id, @review_submission.team&.to_s, @review_submission.user.display_name]
    end    
  end

  protected
  def find_review
    @review = ReviewFeedback.find_by(id: params[:id])
    if @review.nil? || @review.submission_id != @submission.id
      redirect_back fallback_location: course_assignment_submission_path(params[:course_id], params[:assignment_id], params[:submission_id]),
                    alert: "No such review"
      return
    end
  end
end
