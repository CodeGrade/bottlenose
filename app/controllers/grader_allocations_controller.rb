class GraderAllocationsController < ApplicationController
  layout 'course'

  before_action :find_course
  before_action :find_assignment, except: [:stats, :abandon, :delete]
  before_action :find_grader, except: [:stats, :abandon, :delete, :abandon_all, :delete_all]
  before_action :require_current_user
  before_action -> { require_admin_or_staff(course_assignment_path(@course, @assignment)) }, only: [:index]
  before_action -> { require_admin_or_assistant(course_assignment_path(@course, @assignment)) },
                only: [:patch, :edit, :update, :abandon, :delete, :abandon_all, :delete_all]
  before_action -> { require_admin_or_prof(course_path(@course)) }, only: [:stats]

  def index
    compute_who_grades
  end

  def edit
    compute_who_grades
    @unfinished, @finished = @who_grades[nil].partition {|s| @grades[s.id].score.nil?}
  end

  def patch
    existing =
      GraderAllocation
      .where(assignment: @assignment)
      .where(submission_id: params[:submission_id])
      .where.not(abandoned: true)
    if existing.count > 0
      redirect_back fallback_location: edit_course_assignment_grader_allocations_path(@course, @assignment, @grader),
                    alert: ("This submission is already assigned to #{existing.first.grader.name}.  " +
                            "Please abandon or delete it first.")
      return
    end
    sub = (params[:submission_id] && Submission.find_by(id: params[:submission_id]))
    if sub.nil? || (sub.assignment_id != @assignment.id)
      redirect_back fallback_location: edit_course_assignment_grader_allocations_path(@course, @assignment, @grader),
                    alert: "This submission does not belong to this assignment."
      return
    end
    who_grades = (params[:who_grades_id] && User.find_by(id: params[:who_grades_id]))
    if who_grades.nil?
      redirect_back fallback_location: edit_course_assignment_grader_allocations_path(@course, @assignment, @grader),
                    alert: "The specified grader does not exist"
      return
    elsif !who_grades.course_staff?(@course)
      redirect_back fallback_location: edit_course_assignment_grader_allocations_path(@course, @assignment, @grader),
                    alert: "#{grader.name} is not registered as staff for this course"
      return
    end

    alloc = GraderAllocation.find_or_initialize_by(
      submission_id: params[:submission_id],
      who_grades_id: who_grades.id,
      assignment: @assignment,
      course: @course)
    alloc.grading_assigned = DateTime.now
    alloc.abandoned = false
    alloc.save
    redirect_back fallback_location: edit_course_assignment_grader_allocations_path(@course, @assignment, @grader)
  end

  def update
    total_weight = params[:weight].values.map(&:to_f).sum
    if total_weight == 0
      redirect_back fallback_location: edit_course_assignment_grader_allocations_path(@course, @assignment, @grader),
                    alert: "Total weight for all graders is zero; cannot allocate any work!"
      return
    end
    compute_who_grades
    unfinished, finished = @who_grades[nil].partition {|s| @grades[s.id].score.nil?}
    ungraded_count = unfinished.count
    weights = {}
    who_grades = @course.staff.to_a
    params[:weight].permit(who_grades.map{|s| s.id.to_s}).to_h.map do |k, v|
      weights[k.to_i] = (v.to_f / total_weight)
    end
    unfinished.shuffle!
    time = DateTime.now
    who_grades.sort_by! {|g| 0 - weights[g.id] || 0}
    GraderAllocation.transaction do
      who_grades.each do |g|
        1.upto(weights[g.id] * ungraded_count) do |i|
          sub = unfinished.pop
          alloc = GraderAllocation.find_or_initialize_by(
            submission: sub,
            who_grades_id: g.id,
            assignment: @assignment,
            course: @course)
          alloc.grading_assigned = time
          alloc.abandoned = false
          alloc.save
        end
      end
      unfinished.each_with_index do |sub, i|
        g = who_grades[i % unfinished.count]
        alloc = GraderAllocation.find_or_initialize_by(
          submission: sub,
          who_grades_id: g.id,
          assignment: @assignment,
          course: @course)
        alloc.grading_assigned = time
        alloc.abandoned = false
        alloc.save
      end
    end
    redirect_back fallback_location: edit_course_assignment_grader_allocations_path(@course, @assignment, @grader)
  end

  def abandon
    alloc = GraderAllocation.find_by(id: params[:id])
    if alloc.nil? || (alloc.course_id != @course.id)
      redirect_back fallback_location: root_path, alert: "No such grader allocation for this course"
      return
    end
    alloc.abandoned = true
    alloc.grading_completed = DateTime.now
    alloc.save
    if params[:config]
      redirect_back fallback_location: edit_course_assignment_grader_allocations_path(@course, alloc.assignment_id, params[:config])
    else
      redirect_back fallback_location: course_assignment_path(@course, alloc.assignment_id)
    end
  end
  
  def delete
    alloc = GraderAllocation.find_by(id: params[:id])
    if alloc.nil? || (alloc.course_id != @course.id)
      redirect_back fallback_location: root_path, alert: "No such grader allocation for this course"
      return
    end
    alloc.destroy
    if params[:config]
      redirect_back fallback_location: edit_course_assignment_grader_allocations_path(@course, alloc.assignment_id, params[:config])
    else
      redirect_back fallback_location: course_assignment_path(@course, alloc.assignment_id)
    end
  end

  def abandon_all
    allocs = @assignment.grader_allocations.where(grading_completed: nil)
    if params[:staff]
      allocs = allocs.where(who_grades_id: params[:staff])
    end
    allocs.update_all(abandoned: true, grading_completed: DateTime.now)
    if params[:config]
      redirect_back fallback_location: edit_course_assignment_grader_allocations_path(@course, @assignment, params[:config])
    else
      redirect_back fallback_location: course_assignment_path(@course, @assignment)
    end
  end
  def delete_all
    allocs = @assignment.grader_allocations.where(grading_completed: nil)
    if params[:staff]
      allocs = allocs.where(who_grades_id: params[:staff])
    end
    allocs.destroy_all
    if params[:config]
      redirect_back fallback_location: edit_course_assignment_grader_allocations_path(@course, @assignment, params[:config])
    else
      redirect_back fallback_location: course_assignment_path(@course, @assignment)
    end
  end

  def compute_who_grades
    @allocations =
      multi_group_by(GraderAllocation
                      .where(assignment: @assignment)
                      .joins("INNER JOIN users ON users.id = grader_allocations.who_grades_id"),
                     [:who_grades_id, :submission_id], true)
    @grades = 
      # only use submissions that are being used for grading, but this may produce duplicates for team submissions
      # only pick submissions from this course
      # only pick non-staff submissions
      # sort the assignments
      Grade
      .joins("INNER JOIN used_subs ON grades.submission_id = used_subs.submission_id")
      .joins("INNER JOIN registrations ON used_subs.user_id = registrations.user_id")
      .where("used_subs.assignment_id": @assignment.id)
      .select("grades.*")
      .where(grader: @grader)
      .joins("INNER JOIN users ON used_subs.user_id = users.id")
      .select("users.name AS user_name")
      .where("registrations.role": Registration::roles["student"])
      .order("users.name")
      .map{|g| [g.submission_id, g]}
      .to_h
    @used_subs =
      @assignment
      .used_submissions
      .joins("INNER JOIN registrations ON used_subs.user_id = registrations.user_id")
      .where("registrations.role": Registration::roles["student"])
    @invert_used_subs = UsedSub.where(assignment: @assignment).includes(:user).group_by(&:submission_id)
    @who_grades = {}
    # @who_grades =
    #   GraderAllocation
    #   .where(assignment: @assignment)
    #   .joins(SubsForGrading.where(assignment: @assignment))
    #   .joins("INNER JOIN users ON users.id = grader_allocations.who_grades_id")
    #   .group_by(&:who_grades_id)
    subs_and_graders =
      @used_subs
      .includes(:user).includes(team: [:users])
      .joins("LEFT OUTER JOIN grader_allocations ga ON used_subs.submission_id = ga.submission_id")
      .select("submissions.*", "ga.who_grades_id", "ga.abandoned")
    @who_grades = subs_and_graders.group_by(&:who_grades_id)
    subs_and_graders = subs_and_graders.group_by(&:id)
    @course.staff.each do |g|
      @who_grades[g.id] = [] unless @who_grades[g.id]
    end
    @who_grades[nil] = [] unless @who_grades[nil]
    subs_and_graders.each do |sub_id, subs|
      if subs.all?(&:abandoned)
        @who_grades[nil].push subs.first
      end
    end

    if @grader
      unknown = @who_grades[nil]
      @who_grades[nil] = []
      grading_guesses = @grader.guess_who_graded unknown
      if grading_guesses.nil?
        @who_grades[nil] = unknown
      else
        unknown.each do |sub|
          uid = grading_guesses[sub.id]
          @who_grades[uid].push sub # pushes back onto @who_grades[nil] if unknown
        end
      end
    end
  end

  def stats
    @grader_info = GraderAllocation.where(course: @course).group_by(&:who_grades_id).map do |g, gas|
      notdone, finished = gas.partition{|ga| ga.abandoned? || ga.grading_completed.nil?}
      abandoned, incomplete = notdone.partition{|ga| ga.abandoned?}
      avg_grading_time = finished.map{|ga| (ga.grading_completed - ga.grading_assigned) / 1.day.seconds }.sum
      if finished.count > 0
        avg_grading_time = avg_grading_time / finished.count
      end
      [g, {abandoned: abandoned.count, incomplete: incomplete.count, avg_grading_time: avg_grading_time}]
    end.to_h
  end
  
  def find_grader
    @grader = Grader.find_by(id: params[:grader_id])
    if @grader.nil? || (@grader.assignment_id != @assignment.id)
      redirect_back fallback_location: course_path(@course), alert: "No such grader for this assignment"
      return
    elsif @grader.autograde?
      redirect_back fallback_location: course_path(@course), alert: "That grader is automatic; no allocations needed"
      return
    end
  end
end
