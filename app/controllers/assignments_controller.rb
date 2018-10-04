require 'sub_tarball'

class AssignmentsController < ApplicationController
  layout 'course'

  before_action :find_course
  before_action -> { find_assignment(params[:id]) }, except: [:index, :new, :create, :edit_weights, :update_weights]
  before_action :require_registered_user
  before_action -> { require_admin_or_prof(course_assignments_path) },
                only: [:edit, :edit_weights, :update, :update_weights,
                       :new, :create, :destroy, :recreate_grades]
  before_action :require_admin_or_assistant, only: [:tarball, :publish]

  def show
    admin_view = current_user_site_admin? || current_user_staff_for?(@course)
    if admin_view
      submissions = @assignment.used_submissions.includes(:user).order(created_at: :desc).to_a
      no_missing_grades =
        (Grade.where(submission: submissions, grader: @assignment.graders).count ==
         submissions.count * @assignment.graders.count)
      @all_complete = no_missing_grades && (Grade.where(submission: submissions, score: nil).count == 0)
    elsif @assignment.nil? or @assignment.available > DateTime.now
      redirect_back fallback_location: course_assignments_path, alert: "No such assignment exists or is available"
      return
    else
      submissions = current_user.submissions_for(@assignment).includes(:user).order(created_at: :desc).to_a
    end
    @gradesheet = Gradesheet.new(@assignment, submissions)

    @students = @course.students
                .select("users.*", "registrations.dropped_date", "registrations.id as reg_id")
                .map{|s| [s.id, s]}.to_h
    @sections_by_student = RegistrationSection.where(registration: @course.registrations).group_by(&:registration_id)
    @section_crns = @course.sections.map{|sec| [sec.id, sec.crn]}.to_h

    self.send("show_#{@assignment.type.capitalize}")
    if admin_view
      render "show_#{@assignment.type.underscore}"
    else
      @user = current_user
      @team = @user.active_team_for(@course, @assignment)
      render "show_user_#{@assignment.type.underscore}"
    end
  end

  def index
    @ordered_assignments = @course.assignments.order(due_date: :desc, available: :desc)
    @stats = Submission.joins(:used_subs).where(assignment: @ordered_assignments)
             .select("min(submissions.assignment_id) as a_id")
             .select("min(score), avg(score), max(score)")
             .group("submissions.assignment_id").map{|a| [a.a_id, a]}.to_h
  end

  def new
    assns = @course.assignments.order(created_at: :desc)
    last_assns = assns.group_by(&:type).map{|t, as| [t, as.first]}.to_h
    ["Files", "Exam", "Questions", "Codereview"].each {|type| last_assns[type] ||= assns.first }
    if @assignment.is_a? Files
      @files = @assignment
    else
      @files = Files.new(course_id: @course.id,
                         due_date: (Time.now + 1.week).end_of_day.strftime("%Y/%m/%d %H:%M"),
                         available: Time.now.strftime("%Y/%m/%d %H:%M"),
                         lateness_config_id: @course.lateness_config_id,
                         points_available: last_assns["Files"]&.points_available,
                         request_time_taken: true)
    end

    if @assignment.is_a? Exam
      @exam = @assignment
    else
      @exam = Exam.new(course_id: @course.id,
                       due_date: (Time.now + 1.week).end_of_day.strftime("%Y/%m/%d %H:%M"),
                       available: Time.now.strftime("%Y/%m/%d %H:%M"),
                       lateness_config_id: @course.lateness_config_id,
                       points_available: last_assns["Exam"]&.points_available,
                       request_time_taken: false)
      @exam.graders = [ExamGrader.new(order: 1)]
    end

    if @assignment.is_a? Questions
      @quest = @assignment
    else
      @quest = Questions.new(course_id: @course.id,
                             due_date: (Time.now + 1.week).end_of_day.strftime("%Y/%m/%d %H:%M"),
                             available: Time.now.strftime("%Y/%m/%d %H:%M"),
                             lateness_config_id: @course.lateness_config_id,
                             points_available: last_assns["Questions"]&.points_available,
                             request_time_taken: false)
      @quest.graders = [QuestionsGrader.new(order: 1)]
    end

    if @assignment.is_a? Codereview
      @codereview = @assignment
    else
      @codereview = Codereview.new(course_id: @course.id,
                                   due_date: (Time.now + 1.week).end_of_day.strftime("%Y/%m/%d %H:%M"),
                                   available: Time.now.strftime("%Y/%m/%d %H:%M"),
                                   lateness_config_id: @course.lateness_config_id,
                                   points_available: last_assns["Codereview"]&.points_available,
                                   request_time_taken: false)
      @codereview.graders = [CodereviewGrader.new(order: 1)]
    end

    @assignment = @files if @assignment.nil?
    @legal_actions = @files.legal_teamset_actions.reject{|k, v| v.is_a? String}
  end

  def edit
    @legal_actions = @assignment.legal_teamset_actions.reject{|k, v| v.is_a? String}
  end

  def edit_weights
  end

  def update_weights
    no_problems = true
    params[:weight] = params[:weight] || {}
    assignments = Assignment.where(id: params[:weight].keys).map{|a| [a.id.to_s, a]}.to_h
    if assignments.count != params[:weight].keys.count
      missing = (params[:weight].keys.to_set - assignments.keys.to_set).to_a
      @course.errors.add(:base, "Could not find all requested assignments: " + missing.to_sentence)
      render action: "edit_weights", status: 400
      return
    end
    params[:weight].each do |kv|
      if !(Float(kv[1]) rescue false)
        @course.errors.add(:base, "#{assignments[kv[0]].name} has non-numeric weight `#{kv[1]}`")
        no_problems = false
      end
    end
    unless no_problems
      render action: "edit_weights"
      return
    end
    Assignment.transaction do
      assignments.each do |aid, assn|
        new_weight = params[:weight][aid].to_f
        assn.update_attribute(:points_available, new_weight) if new_weight != assn.points_available
      end
    end
    redirect_to course_assignments_path
  end

  def create
    # Assign the current user to all file uploads for grader configs
    ap = assignment_params
    ap[:graders_attributes]&.each do |k, v|
      v[:upload_by_user_id] = current_user.id
    end
    ap[:course_id] = @course.id
    ap[:blame_id] = current_user.id
    ap[:current_user] = current_user
    if ap[:prevent_late_submissions] == "1" # i.e., true
      ap[:prevent_late_submissions] = ap[:related_assignment_id]
    else
      ap.delete(:prevent_late_submissions)
    end
    @assignment = Assignment.new(ap)

    @assignment.interlocks.each do |lock|
      if lock[:constraint] == "check_section_toggles"
        lock.related_assignment = @assignment
      end
    end

    if @assignment.save
      redirect_to course_assignment_path(@course, @assignment), notice: 'Assignment was successfully created.'
    else
      new_assn = @assignment.dup
      new_assn.lateness_config = @assignment.lateness_config.dup if @assignment.lateness_config.new_record?
      new_assn.graders = @assignment.graders.map(&:dup)
      new_assn.assignment_upload_id = nil # cleanup the Upload and clear out the upload_id, to force re-upload
      new_assn.related_interlocks << @assignment.related_interlocks.map(&:dup)
      new_assn.interlocks << @assignment.interlocks.map(&:dup)
      @assignment.errors.each do |attr, err|
        new_assn.errors[attr] << err
      end
      @assignment.destroy
      @assignment = new_assn
      @legal_actions = @assignment.legal_teamset_actions.reject{|k, v| v.is_a? String}
      new
      render action: "new", status: 400
    end
  end

  def update
    ap = assignment_params
    # Assign the current user to all file uploads for grader configs
    ap[:graders_attributes]&.each do |k, v|
      v[:upload_by_user_id] = current_user.id
    end
    if ap[:prevent_late_submissions] == "1" # i.e., true
      ap[:prevent_late_submissions] = ap[:related_assignment_id]
    else
      ap.delete(:prevent_late_submissions)
    end
    @assignment.assign_attributes(ap)
    @assignment.current_user = current_user

    need_to_unpublish_grades =
      @assignment.graders.any?{|g| g.new_record? || g.changed? || g.marked_for_destruction?}

    @assignment.interlocks.each do |lock|
      if lock[:constraint] == "check_section_toggles"
        lock.related_assignment = @assignment
      end
    end
    
    if @assignment.save
      count = 0
      if need_to_unpublish_grades
        count = @assignment.submissions.where.not(score: nil).update_all(score: nil)
      end
      redirect_to course_assignment_path(@course, @assignment),
                  notice: "Assignment was successfully updated; #{pluralize(count, 'grade')} unpublished."
    else
      @legal_actions = @assignment.legal_teamset_actions.reject{|k, v| v.is_a? String}
      render action: "edit", status: 400
    end
  end

  def destroy
    @assignment.destroy
    redirect_to @course, notice: "Assignment #{params[:id]} has been deleted."
  end

  def show_user
    assignment = Assignment.find_by(id: params[:id])
    if assignment.nil?
      redirect_back fallback_location: course_path(@course), alert: "No such assignment"
      return
    end
    if !current_user
      redirect_back fallback_location: root_path, alert: "Must be logged in"
      return
    elsif current_user_site_admin? || current_user_prof_for?(@course)
      # nothing --- but should this be current_user_staff_for instead?
    elsif current_user.id != params[:user_id].to_i
      redirect_back fallback_location: course_assignment_path(@course, assignment),
                    alert: "Not permitted to see submissions for other students"
      return
    end

    @user = User.find_by(id: params[:user_id])
    if @user.nil?
      redirect_back fallback_location: course_assignment_path(@course, assignment),
                    alert: "No such student"
      return
    end
    subs = @user.submissions_for(assignment)
    submissions = subs.select("submissions.*").includes(:users).order(created_at: :desc).to_a
    @gradesheet = Gradesheet.new(assignment, submissions)
    render "show_user_#{assignment.type.underscore}"
  end

  def tarball
    tb = SubTarball.new(params[:id])
    if params[:moss]
      tb.update_moss!
    else
      tb.update!
    end
    send_data File.read(tb.full_path),
              filename: File.basename(tb.full_path),
              disposition: "attachment",
              type: "application/x-gzip"
    FileUtils.rm_rf(tb.full_path)
  end

  def publish
    publish_all = (params[:all] == "true")
    used = @assignment.used_submissions
    count = 0
    used.each do |u|
      ungraded = u.grades.where(score: nil)
      if !publish_all && ungraded.count > 0
        next
      end
      ungraded.each do |g| g.grader.grade(@assignment, u) end
      u.grades.update_all(:available => true)
      u.compute_grade!
      count += 1
    end

    redirect_back fallback_location: course_assignment_path(@course, @assignment),
                  notice: "#{pluralize(count, 'grade')} successfully published"
  end

  def recreate_grades
    count = do_recreate_grades @assignment
    redirect_back fallback_location: course_assignment_path(@course, @assignment),
                  notice: "#{pluralize(count, 'grade')} created"
  end




  protected

  def do_recreate_grades(assignment)
    confs = assignment.graders.to_a
    count = @assignment.used_submissions.reduce(0) do |sum, sub|
      sum + sub.recreate_missing_grades(confs)
    end
    count
  end

  def assignment_params
    params[:assignment].permit(:name, :assignment, :due_date, :available,
                               :points_available, :hide_grading, :blame_id,
                               :assignment_file,  :type, :related_assignment_id,
                               :course_id, :team_subs, :request_time_taken,
                               :removefile, :extra_credit,
                               :teamset_plan, :teamset_source_use, :teamset_source_copy,
                               :prevent_late_submissions,
                               interlocks_attributes: [
                                 :id, :_destroy, :constraint, :related_assignment_id
                               ],
                               lateness_config_attributes: [
                                 :type, :percent_off, :frequency,
                                 :max_penalty, :days_per_assignment,
                                 :_destroy, :id
                               ],
                               graders_attributes: [
                                 :avail_score, :upload_file, :extra_upload_file, :params,
                                 :type, :id, :_destroy, :errors_to_show, :test_class,
                                 :review_target, :review_count, :review_threshold,
                                 :upload_by_user_id, :order, :line_length, :extra_credit,
                                 :removefile
                               ]
                              )
  end


  ####################
  # Per-assignment-type actions, by action

  # Show
  def show_Files
  end
  def show_Questions
    @questions = @assignment.questions
  end
  def show_Exam
    @questions = @assignment.questions
    @grader = @assignment.graders.first
  end

  def show_Codereview
    @questions = @assignment.questions
  end
end
