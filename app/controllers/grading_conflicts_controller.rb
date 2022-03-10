class GradingConflictsController < ApplicationController
  
  layout 'course'

  before_action :find_course
  before_action :require_registered_user
  before_action :require_admin_or_prof, only: [:delete, :update]

  def index
    if current_user.course_professor?(@course) || current_user.site_admin
      @grading_conflicts = GradingConflict.where(course: @course)
    elsif current_user.course_assistant?(@course)
      @grading_conflicts = GradingConflict.where(course: @course, staff: current_user)
    else
      @grading_conflicts = GradingConflict.where(course: @course, student: current_user)
    end
  end

  def show
    find_grading_conflict
  end

  def new
    @tas_and_graders = Registration.where(course: @course)
      .where(role: Registration.roles[:grader])
      .or(Registration.where(role: Registration.roles[:assistant]))
      .map{|reg| reg.user}
    @students = Registration.where(course: @course, role: Registration.roles[:student])
      .map{|reg| reg.user}
  end

  # TODO: Use find_or_initialize_by in case of student submitting GradingConflict request
  # a second time.
  def create
    if current_user.professor_ever? || current_user.site_admin?
      @grading_conflict = GradingConflict.find_or_initialize_by(student_id: gc_params[:student_id], staff_id: gc_params[:staff_id],
        course: @course)
    elsif current_user.course_grader?(@course) || current_user.course_assistant?(@course)
      @grading_conflict = GradingConflict.find_or_initialize_by(student_id: gc_params[:student_id], staff: current_user, course: @course)
      @grading_conflict.status = :pending
    else
      @grading_conflict = GradingConflict.find_or_initialize_by(student: current_user, staff_id: gc_params[:staff_id], course: @course)
      @grading_conflict.status = :pending
    end

    # TODO: Add audit information to creation of Grading Conflict
    @grading_conflict.grading_conflict_audits << GradingConflictAudit.create(grading_conflict: @grading_conflict,
      user: current_user, status: @grading_conflict.status)

    if @grading_conflict.save!
      redirect_to course_grading_conflicts_path(@course), 
            notice: "Successfully created a grading conflict."
    else
      redirect_back new_course_grading_conflict_path(@course),
            alert: "Error saving the grading conflict."
    end
  end

  private

  def gc_params
    ans = params.require(:grading_conflict).permit(:student_id, :staff_id)
    if params[:grading_conflict].key?(:student_id)
      ans[:student_id] = params[:grading_conflict][:student_id]
    end
    if params[:grading_conflict].key?(:staff_id)
      ans[:student_id] = params[:grading_conflict][:student_id]
    end
    ans
  end

  def find_grading_conflict
    @grading_conflict = GradingConflict.find_by(id: params[:id])
    if @grading_conflict.nil? || @course.id != @grading_conflict.course_id
      redirect_to course_grading_conflicts_path(@course),
        alert: "No such grading conflict for this course."
      return
    end
  end
end
