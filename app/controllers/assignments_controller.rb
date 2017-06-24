class AssignmentsController < ApplicationController
  layout 'course'

  before_action :find_course
  before_action :require_registered_user
  before_action :find_assignment, except: [:index, :new, :create, :edit_weights, :update_weights]
  before_action :require_admin_or_prof, only: [:edit, :edit_weights, :update, :update_weights,
                                               :new, :create, :destroy,
                                               :recreate_grades]
  before_action :require_admin_or_staff, only: [:tarball, :publish]

  def show
    admin_view = current_user_site_admin? || current_user_staff_for?(@course)
    if admin_view
      submissions = @assignment.used_submissions.includes(:user).order(created_at: :desc).to_a
      @all_complete = (Grade.where(submission: submissions, score: nil).count == 0)
    elsif @assignment.nil? or @assignment.available > DateTime.now
      redirect_back fallback_location: course_assignments_path, alert: "No such assignment exists or is available"
      return
    else
      submissions = current_user.submissions_for(@assignment).includes(:user).order(created_at: :desc).to_a
    end
    @gradesheet = Gradesheet.new(@assignment, submissions)


    self.send("show_#{@assignment.type.capitalize}")
    if admin_view
      render "show_#{@assignment.type.underscore}"
    else
      @user = current_user
      render "show_user_#{@assignment.type.underscore}"
    end
  end

  def index
  end

  def new
    @files = Assignment.new
    @files.course_id = @course.id
    @files.due_date = (Time.now + 1.week).end_of_day.strftime("%Y/%m/%d %H:%M")
    @files.available = Time.now.strftime("%Y/%m/%d %H:%M")
    @files.lateness_config_id = @course.lateness_config_id
    @files.request_time_taken = true

    @exam = @files.dup
    @exam.graders = [ExamGrader.new]
    @files.lateness_config_id = @course.lateness_config_id
    @exam.request_time_taken = false

    @quest = @files.dup
    @files.lateness_config_id = @course.lateness_config_id
    @quest.request_time_taken = false

    last_assn = @course.assignments.order(created_at: :desc).first
    if last_assn
      @files.points_available = last_assn.points_available
      @exam.points_available  = last_assn.points_available
      @quest.points_available = last_assn.points_available
    end

    @legal_actions = @files.legal_teamset_actions.reject{|k, v| v.is_a? String}
  end

  def edit
    @legal_actions = @assignment.legal_teamset_actions.reject{|k, v| v.is_a? String}
  end

  def edit_weights
  end

  def update_weights
    no_problems = true
    params[:weight].each do |kv|
      if !(Float(kv[1]) rescue false)
        @course.errors.add(:base, "#{Assignment.find(kv[0]).name} has non-numeric weight `#{kv[1]}`")
        no_problems = false
      end
    end
    unless no_problems
      render action: "edit_weights"
      return
    end
    params[:weight].each do |kv|
      Assignment.find(kv[0]).update_attribute(:points_available, kv[1])
    end
    redirect_to course_assignments_path
  end

  def create
    @assignment = Assignment.new(assignment_params)
    @assignment.course_id = @course.id
    @assignment.blame_id = current_user.id
    if @assignment.type == "exam"
      @assignment.available = @assignment.due_date
    end

    if @assignment.save
      @assignment.save_uploads! if params[:assignment][:assignment_file]
      redirect_to course_assignment_path(@course, @assignment), notice: 'Assignment was successfully created.'
    else
      @assignment.destroy
      render action: "new"
    end
  end

  def update
    ap = assignment_params
    if params[:assignment][:removefile] == "remove"
      ap[:assignment_file] = nil
      @assignment.assignment_upload_id = nil
    end

    # Assign the current user to all file uploads for grader configs
    ap[:graders_attributes]&.each do |k, v|
      v[:upload_by_user_id] = current_user.id
    end
    @assignment.assign_attributes(ap)

    if @assignment.save
      @assignment.save_uploads! if ap[:assignment_file]
      redirect_to course_assignment_path(@course, @assignment), notice: 'Assignment was successfully updated.'
    else
      @legal_actions = @assignment.legal_teamset_actions.reject{|k, v| v.is_a? String}
      render action: "edit"
    end
  end

  def destroy
    @assignment.destroy
    redirect_to @course, notice: "Assignment #{params[:id]} has been deleted."
  end

  def show_user
    assignment = Assignment.find(params[:id])
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

    @user = User.find(params[:user_id])
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
    redirect_to tb.path
  end

  def publish
    publish_all = (params[:all] == "true")
    used = @assignment.used_submissions
    used.each do |u|
      ungraded = u.grades.where(score: nil)
      if !publish_all && ungraded.count > 0
        next
      end
      ungraded.each do |g| g.grader.grade(@assignment, u) end
      u.grades.update_all(:available => true)
      u.compute_grade!
    end

    redirect_back fallback_location: course_assignment_path(@course, @assignment),
                  notice: 'Grades successfully published'
  end

  def recreate_grades
    count = do_recreate_grades @assignment
    redirect_back fallback_location: course_assignment_path(@course, @assignment),
                  notice: "#{plural(count, 'grade')} created"
  end




  protected

  def set_grader
    return self.send("set_#{params[:assignment][:type]}_graders")
  end


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
                               :lateness_config_id, :removefile,
                               :teamset_plan, :teamset_source_use, :teamset_source_copy,
                               lateness_config_attributes: [
                                 :type, :percent_off, :frequency,
                                 :max_penalty, :days_per_assignment,
                                 :_destroy
                               ],
                               graders_attributes: [
                                 :avail_score, :upload_file, :params,
                                 :type, :id, :_destroy, :errors_to_show, :test_class,
                                 :upload_by_user_id
                               ]
                              )
  end

  def graders_params
    graders = params.to_unsafe_h["graders"] # FIXME: Nested models refactor.

    if graders.nil?
      nil
    else
      graders.map do |k, v|
        [k, v]
      end.to_h
    end
  end

  def require_admin_or_prof
    unless current_user_site_admin? || current_user_prof_for?(@course)
      redirect_back fallback_location: course_assignments_path, alert: "Must be an admin or professor."
      return
    end
  end

  def require_admin_or_staff
    unless current_user_site_admin? || current_user_staff_for?(@course)
      redirect_to root_path, alert: "Must be an admin or staff."
      return
    end
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

  def set_files_graders
    params_graders = graders_params
    if params_graders.nil?
      @assignment.errors.add(:graders, "parameter is missing")
      return false
    end

    no_problems = true
    graders = {}
    params_graders.each do |id, grader|
      grader[:removed] = true if grader[:removed] == "true"
      grader[:removed] = false if grader[:removed] == "false"
      if grader[:removed]
        graders[id] = grader
        next
      end

      type = grader[:type]
      if type.nil?
        @assignment.errors.add(:graders, "type is missing")
        no_problems = false
        next
      end
      type = type.split("_")[1]

      grader = grader[type]
      grader[:type] = type

      upload = grader[:upload_file]
      if upload
        up = Upload.new
        up.user_id = current_user.id
        up.store_upload!(upload, {
                           type: "#{type} Configuration",
                           date: Time.now.strftime("%Y/%b/%d %H:%M:%S %Z"),
                           mimetype: upload.content_type
                         })
        unless up.save
          @assignment.errors.add(:upload_file, "could not save upload")
          no_problems = false
          next
        end
        grader[:upload_file] = up
      end

      graders[id] = grader
    end
    return no_problems unless no_problems


    Grader.transaction do
      existing_confs = @assignment.graders.to_a
      existing_ags = @assignment.assignment_graders.to_a
      max_order = existing_ags.reduce(0) do |acc, ag| [acc, ag.order].max end
      existing_confs.each do |c|
        conf = graders[c.id.to_s]
        graders.delete c.id.to_s
        if conf[:removed]
          existing_ags.find{|g| g.grader_id == c.id}.destroy
          next
        else
          c.assign_attributes(conf)
          if c.changed?
            unless c.save
              @assignment.errors << c.errors
              no_problems = false
              raise ActiveRecord::Rollback
            end
            # make sure that the graders are updated to be the right total score
            unless c.autograde?
              Grader.where(grader_id: c.id).update_all({out_of: c.avail_score})
            end
          end
        end
      end

      graders.each do |k, conf|
        next if conf[:removed]
        c = Grader.new(conf)
        if c.invalid? or !c.save
          no_problems = false
          @assignment.errors.add(:graders, "Could not create grader #{c.to_s}")
          raise ActiveRecord::Rollback
        else
          AssignmentGrader
            .find_or_initialize_by(assignment_id: @assignment.id, grader_id: c.id)
            .update(order: max_order)
          max_order += 1
        end
      end

      # NOT SURE IF I WANT TO DO THIS IMMEDIATELY OR NOT
      # do_recreate_graders @assignment
    end

    return no_problems
  end

  def find_assignment
    @assignment = Assignment.find_by(id: params[:id])
    if @assignment.nil?
      redirect_back fallback_location: course_assignments_path, alert: "No such assignment"
      return
    end
    if @assignment.course_id != params[:course_id].to_i
      redirect_back fallback_location: course_assignments_path, alert: "No such assignment for this course"
      return
    end
  end
end
