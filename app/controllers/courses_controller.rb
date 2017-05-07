require 'csv'
require 'course_spreadsheet'

class CoursesController < ApplicationController
  layout 'course'

  before_action :require_current_user, except: [:public]
  before_action :load_and_verify_course_registration, except: [:public]
  before_action :require_admin_or_prof, only: [:update, :edit, :gradesheet]

  def index
    @courses_by_term = Course.order(:name).group_by(&:term)

    # We can't use the course layout if we don't have a @course.
    render layout: 'application'
  end

  def show
    if current_user_staff_for?(@course)
      @pending_grading =
        # only use submissions that are being used for grading, but this may produce duplicates for team submissions
        # only pick submissions from this course
        # only pick non-staff submissions
        # hang on to the assignment id
        # only keep unfinished graders
        # sort the assignments
        Grade
        .joins("INNER JOIN used_subs ON grades.submission_id = used_subs.submission_id")
        .joins("INNER JOIN assignments ON used_subs.assignment_id = assignments.id")
        .joins("INNER JOIN registrations ON used_subs.user_id = registrations.user_id")
        .where("assignments.course_id": @course.id)
        .select("grades.*", "used_subs.assignment_id", "assignments.due_date")
        .joins("INNER JOIN users ON used_subs.user_id = users.id")
        .distinct.select("users.name AS user_name")
        .where(score: nil)
        .where("registrations.role": Registration::roles["student"])
        .order("assignments.due_date", "users.name")
        .group_by{|r| r.assignment_id}
    else
      @pending_grading = {}
    end
    # elsif @registration.staff?
    #   @pending_grading = []
    #   @assignments = []
    @allocated_grading =
      GraderAllocation
      .where(grade_id: current_user.id)
      .where(course: @course)
      .where(grading_completed: nil)
      .group_by(&:assignment_id)
    @completed_grading =
      GraderAllocation
      .where(grade_id: current_user.id)
      .where(course: @course)
      .where.not(grading_completed: nil)
      .joins(:submission)
      .where("submissions.score": nil)
      .group_by(&:assignment_id)
    @assignments = Assignment
                   .where(id: @pending_grading.keys + @allocated_grading.keys + @completed_grading.keys)
                   .map{|a| [a.id, a]}.to_h

    if current_user_prof_for?(@course)
      @abnormals = {}
      people = @course.users_with_drop_info.where("registrations.dropped_date IS ?", nil).to_a
      @assns = @course.assignments.to_a.sort_by(&:due_date)
      all_subs = Assignment.submissions_for(people, @assns).group_by(&:assignment_id)
      used_subs = UsedSub.where(assignment_id: @assns.map(&:id)).group_by(&:assignment_id)
      @assns.each do |a|
        next unless all_subs[a.id]
        a_subs = all_subs[a.id].group_by(&:for_user)
        used = used_subs[a.id]
        people.each do |p|
          subs = a_subs[p.id]
          if (subs and subs.count > 0) and (used.nil? or used.find{|us| us.user_id == p.id}.nil?)
            @abnormals[a] = [] unless @abnormals[a]
            @abnormals[a].push p
          end
        end
      end
      @unpublished =
        Submission
        .joins("INNER JOIN used_subs ON submissions.id = used_subs.submission_id")
        .where("used_subs.assignment_id": @assns.map(&:id))
        .joins("INNER JOIN grades ON grades.submission_id = submissions.id")
        .where.not("grades.score": nil)
        .where("grades.available": false)
        .joins("INNER JOIN users ON used_subs.user_id = users.id")
        .order("users.name")
        .select("DISTINCT submissions.*", "users.name AS user_name")
    end
  end

  def new
    new_with_errors nil
  end

  def edit
    prep_sections
  end

  def facebook
    unless current_user_site_admin? || current_user_staff_for?(@course)
      redirect_back fallback_location: root_path, alert: "Must be an admin or professor."
      return
    end
    prep_sections
    @students = @course.students
                .select("users.*", "registrations.dropped_date", "registrations.section_id")
                .order("users.last_name", "users.first_name")
  end

  def prep_sections
    @sections = CourseSection.where(course: @course).to_a
    if @sections.count == 0
      @sections = [CourseSection.new(course: @course, instructor: current_user)]
    end
  end

  def create
    unless current_user_site_admin? || current_user_prof_ever?
      redirect_to(root_path, alert: 'Must be an admin or professor to create a course.')
      return
    end

    @course = Course.new(course_params)

    unless course_section_params.count > 0
      @course.errors[:base] << "Need to create at least one section"
      new_with_errors @course.errors
      return
    end

    if set_default_lateness_config and @course.save and create_sections
      redirect_to course_path(@course), notice: 'Course was successfully created.'
    else
      @course.destroy unless @course.new_record?
      new_with_errors @course.errors
      return
    end
  end


  def update
    @course.assign_attributes(course_params)

    if set_default_lateness_config and create_sections and @course.save
      redirect_to course_path(@course), notice: 'Course was successfully updated.'
    else
      prep_sections
      render :edit, layout: 'application'
    end
  end

  # def destroy
  #   @course.destroy
  #   redirect_to courses_path
  # end

  def public
    @course = Course.find_by(id: params[:id])

    if @course.nil?
      redirect_to root_path, alert: "No such course or that material is not public."
      return
    end
    if current_user
      redirect_to course_path(@course)
      return
    end

    unless @course.public?
      redirect_to root_path, alert: "No such course or that material is not public."
      return
    end
  end

  def withdraw
    @course = Course.find_by(id: params[:id])

    reg = current_user.registration_for(@course)
    if reg.nil?
      redirect_to course_path(@course), alert: "You cannot withdraw from a course you aren't registered for."
      return
    elsif reg.dropped_date
      redirect_to course_path(@course), alert: "You have already withdrawn from this course."
      return
    elsif reg.role == "professor" && @course.professors.count == 1
      redirect_to course_path(@course), alert: "You cannot withdraw from the course: you are the only instructor for it."
      return
    end
    reg.dropped_date = DateTime.now
    reg.show_in_lists = false
    reg.save!
    current_user.disconnect(@course)
    redirect_to root_path, notice: "You have successfully withdrawn from #{@course.name}"
  end

  def gradesheet
    @all_course_info = all_course_info
    respond_to do |format|
      #      format.csv { send_data @all_course_info.to_csv(col_sep: "\t") }
      format.xlsx { send_data @all_course_info.to_xlsx, type: "application/xlsx" }
    end
  end

  protected

  def all_course_info
    CourseSpreadsheet.new(@course)
  end

  def new_with_errors(errs)
    @course = Course.new
    if errs
      merge_errors @course.errors, errs
    end
    prep_sections
    # We can't use the course layout if we don't have a @course.
    render :new, layout: 'application'
  end
  
  def load_and_verify_course_registration
    # We can't find the course for the action 'courses#index'.
    if controller_name == 'courses' &&
       (action_name == 'index' ||
        action_name == 'new' ||
        action_name == 'create')
      return
    end

    @course = Course.find_by(id: params[:course_id] || params[:id])

    if @course.nil?
      redirect_to courses_url, alert: "No such course"
      return
    end

    if current_user_site_admin?
      return
    end

    # not sure why this is needed, given that require_current_user ought to have been called :(
    if current_user.nil?
      redirect_to root_path, alert: "You need to log in first"
      return
    end

    @registration = current_user.registration_for(@course)
    if @registration.nil?
      redirect_to courses_url, alert: "You're not registered for that course."
      return
    elsif @registration.dropped_date
      redirect_to courses_url, alert: "You've already dropped that course."
      return
    end
  end

  def course_params
    params[:course].permit(:name, :footer, :total_late_days, :private, :public, :course_section,
                           :term_id, :sub_max_size)
  end

  def course_section_params
    cs = params[:course][:course_section]
    if cs
      cs.values
    else
      {}
    end    
  end


  def set_default_lateness_config
    lateness = params[:lateness]

    if lateness.nil?
      @course.errors.add(:lateness, "Lateness parameter is missing")
      return false
    end
      
    type = lateness[:type]
    if type.nil?
      @course.errors.add(:lateness, "Lateness type is missing")
      return false
    end
    type = type.split("_")[1]
    
    lateness = lateness[type]
    lateness[:type] = type
    if type != "reuse"
      late_config = LatenessConfig.new(lateness.permit(LatenessConfig.attribute_names - ["id"]))
      @course.lateness_config = late_config
      late_config.save
      if type == "FixedDaysConfig"
        @course.total_late_days = course_params[:total_late_days]
      end
    end

    return true
  end

  def create_sections
    sections = course_section_params.map do |sp|
      sec = nil
      if sp[:id]
        sec = CourseSection.find_by(id: sp[:id])
      end
      if sec.nil?
        sec = CourseSection.new
      end
      errs = false
      instructor = User.find_by(username: sp[:instructor])
      if instructor.nil?
        @course.errors[:Section] << "instructor could not be found with username #{sp[:instructor]}"
        errs = true
      end
      if sp[:crn].nil? or sp[:crn].empty?
        @course.errors[:Section] << "CRN must be present"
        errs = true
      else
        Audit.log "Creating section with CRN: #{sp[:crn]}\n"
      end
      if sp[:meeting_time].nil? or sp[:meeting_time].length < 3
        @course.errors[:Section] << "meeting time is missing or too short"
        errs = true
      end
      return if errs
      sec.assign_attributes(instructor: instructor,
                            crn: sp[:crn],
                            meeting_time: sp[:meeting_time],
                            course: @course)
      if sec.save
        sec
      else
        merge_errors @course.errors, sec.errors
        return
      end
    end

    sections.each do |s|
      reg = Registration.find_or_create_by(user: s.instructor,
                                           course: @course,
                                           section: s)
      if reg.nil?
        @course.errors[:base] << reg.errors
        return false
      else
        reg.update_attributes(role: :professor,
                              show_in_lists: false)
      end
    end
    return true
  end
    

  def plural(n, sing, pl = nil)
    if n == 1
      "1 #{sing}"
    elsif pl
      "#{n} #{pl}"
    else
      "#{n} #{sing}s"
    end
  end

  def merge_errors(dest, src)
    src.messages.each do |type, msgs|
      msgs.each do |msg|
        dest[type] << msg
      end
    end
  end

  def require_admin_or_prof
    unless current_user_site_admin? || current_user_prof_for?(@course)
      redirect_back fallback_location: root_path, alert: "Must be an admin or professor."
      return
    end
  end
end
