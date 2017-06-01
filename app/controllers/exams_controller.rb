class ExamsController < ApplicationController
  before_action :find_course
  before_action :set_exam, only: [:show, :edit, :update, :destroy]
  before_action :require_registered_user
  before_action :require_admin_or_prof, only: [:edit, :update, :new, :create]

  # GET /exams
  def index
    @exams = Exam.all
  end

  # GET /exams/1
  def show
    if current_user_site_admin? || current_user_staff_for?(@course)
      staff_show
      return
    end

    @subs = current_user.submissions_for(@assignment).includes(:user).order(created_at: :desc).to_a
  end

  def staff_show
    @subs = @exam.used_submissions.includes(:user).order(created_at: :desc).to_a
    @all_complete = (Grade.where(submission: @subs, score: nil).count == 0)
    @gradesheet = Gradesheet.new(@exam, @subs)
    @grader = @exam.graders.first
    @questions = @exam.questions

    render "staff_show"
  end

  # GET /exams/new
  def new
    @exam = Exam.new
  end

  # GET /exams/1/edit
  def edit
  end

  # POST /exams
  def create
    @exam = Exam.new(exam_params)

    if @exam.save
      redirect_to @exam, notice: 'Exam was successfully created.'
    else
      render :new
    end
  end

  # PATCH/PUT /exams/1
  def update
    if @exam.update(exam_params)
      redirect_to @exam, notice: 'Exam was successfully updated.'
    else
      render :edit
    end
  end

  # DELETE /exams/1
  def destroy
    @exam.destroy
    redirect_to exams_url, notice: 'Exam was successfully destroyed.'
  end

  private
    # Use callbacks to share common setup or constraints between actions.
    def set_exam
      @exam = Exam.find(params[:id])
    end

    # Only allow a trusted parameter "white list" through.
    def exam_params
      params.fetch(:exam, {})
    end
end
