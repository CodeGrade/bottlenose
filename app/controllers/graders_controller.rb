class GradersController < ApplicationController
  before_action :find_course
  before_action :find_assignment
  before_action :find_grader

  def build_log
    unless @grader.status['useOrca']
      return redirect_to course_assignment_path(@course, @assignment),
                         alert: 'This grader does not use Orca'
    end
    @build_status = @grader.status['buildStatus']
    if @build_status.nil? || @build_status == 'Pending'
      return redirect_to course_assignment_path(@course, @assignment),
                         alert: 'This grader is still being built'
    end
    @logs = @grader.status['logs']
    render action: 'build_log'
  end

  private

  def find_grader
    @grader = Grader.find_by_id params[:id]
    return unless @grader.nil?

    redirect_to course_assignment_path(@course, @assignment), alert: 'No such grader'
  end
end
