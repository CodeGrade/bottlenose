class GradersController < ApplicationController
  before_action :find_course
  before_action :find_assignment
  before_action :find_grader

  def build_log
    unless @grader.orca_status
      return redirect_to course_assignment_path(@course, @assignment),
                         alert: 'This grader does not use Orca'
    end
    build_result = @grader.orca_build_result
    if build_result.nil?
      return redirect_to course_assignment_path(@course, @assignment),
                         alert: 'This grader has no build logs from Orca'
    end
    @logs = build_result['logs']
    render action: 'build_log'
  end

  private

  def find_grader
    @grader = Grader.find_by_id params[:id]
    return unless @grader.nil?

    redirect_to course_assignment_path(@course, @assignment), alert: 'No such grader'
  end
end
