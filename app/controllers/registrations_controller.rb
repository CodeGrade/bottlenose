require 'csv'

class RegistrationsController < ApplicationController
  layout 'course'

  before_action :find_course
  before_action :find_registration, except: [:index, :new, :create, :bulk_enter, :bulk_update, :bulk_edit]
  before_action :require_current_user, except: [:public]
  before_action :require_registered_user, except: [:public, :new, :create]
  before_action :require_admin_or_assistant

  def index
    @students = @course.students
    @staff = @course.staff
    @section_crns = @course.sections.map{|sec| [sec.id, sec.crn]}.to_h
    @requests = @course.reg_requests.joins(:user).order('role desc', 'name')
                .includes(:user).includes(:reg_request_sections)
    if current_user.site_admin?
      @role = "professor"
    else
      @role = @registration&.role
    end
  end

  def new
    @registration = Registration.new
  end

  def create
    reg_params = registration_params

    uu = User.find_by(username: reg_params[:username])
    if uu
      if !current_user.site_admin?
        logged_in_role = current_user.registration_for(@course).role
        new_role = reg_params[:role]
        if uu == current_user
          redirect_back fallback_location: course_registrations_path(@course),
            alert: "You are not allowed to create a registration for yourself."
          return
        end
        if new_role != "student" && logged_in_role != "professor"
          redirect_back fallback_location: course_registrations_path(@course),
            alert: "You are not allowed to create #{new_role} registrations."
          return
        end
      end
      @registration = Registration.find_by(course_id: @course, user_id: uu.id)
    end
    if @registration
      new_role = reg_params[:role]
      cur_role = @registration.role
      if Registration.roles[new_role] < Registration.roles[cur_role]
          redirect_back fallback_location: course_registrations_path(@course),
            alert: "You are not allowed to downgrade registrations."
          return
      end
      @registration.assign_attributes(reg_params)
    else
      # Create @registration object for errors.
      @registration = Registration.new(reg_params)
    end

    success = Registration.transaction do
      if @registration && @registration.save && @registration.save_sections
        true
      else
        raise ActiveRecord::Rollback, "Saving registration failed"
      end
    end
    puts "Success is #{success}"
    if success
      redirect_to course_registrations_path(@course),
                  notice: 'Registration was successfully created.'
    else
      render action: :new, status: 400
    end
  end

  def bulk_edit
    @cur_role = (current_user_site_admin? ? "professor" : @registration&.role)
    if params[:role] == "student"
      @registrations = @course.registrations
                       .where(role: Registration::roles["student"])
    elsif @cur_role != "professor"
      redirect_back fallback_location: course_registrations_path(@course),
                    alert: "You are not allowed to edit staff registrations"
      return
    else
      @registrations = @course.registrations
                       .where.not(role: Registration::roles["student"])
    end
    @sections = @course.sections.map{|s| [s.id, s]}.to_h
    @registrations = @registrations.includes(:user)
    @reg_sections = RegistrationSection.where(registration: @registrations).group_by(&:registration_id)
                    .map{|rid, regs| [rid, regs.map{|r| @sections[r.section_id]}]}.to_h
    @registrations = @registrations.to_a.sort_by{|r| r.user.display_name}
  end

  def bulk_update
    respond_to do |f|
      f.json {
        @reg = Registration.find_by(id: params[:id])
        if @reg.nil? || (@reg.course.id != @course.id)
          render :json => {failure: "Unknown registration"}, status: 400
          return
        else
          # might be nil if admin instead of prof
          if (current_user.registration_for(@course)&.role != "professor" && !current_user_site_admin?)
            params.delete(:role) # Only professors may edit roles
          end
          changed = false
          @reg.dropped_date = nil if params[:reenroll]
          @reg.role = params[:role] if params[:role]
          sections = @course.sections.map{|sec| [sec.crn.to_s, sec.id]}.to_h
          section_changes = params[:orig_sections].zip(params[:new_sections])
          section_changes.each do |orig, new|
            rs = RegistrationSection.find_by(registration_id: @reg.id, section_id: sections[orig])
            if rs
              rs.section_id = sections[new]
              if rs.changed?
                changed = true
                rs.save
              end
            end
          end
          if @reg.changed? || changed
            @reg.save
            render :json => {"reg": @reg, "changes": section_changes.map{|o,n| {old: o, new: n}}}
          else
            render :json => {"no-change": true}
          end
        end
      }
      f.html do
        redirect_back(fallback_location: course_registrations_path(@course), notice: "No such page")
      end
    end
  end

  def bulk_enter
    num_added = 0
    failed = []

    CSV.parse(params[:usernames]) do |row|
      uu = User.find_by(username: row[0])
      if uu
        r = Registration.find_by(course_id: @course, user_id: uu.id)
      end
      if r
        r.assign_attributes(course_id: @course.id, new_sections: Section.where(course_id: @course.id,
                                                                               crn: row[1..-1]),
                            username: row[0], role: "student")
      else
        # Create @registration object for errors.
        r = Registration.new(course_id: @course.id, new_sections: Section.where(course_id: @course.id,
                                                                                crn: row[1..-1]),
                             username: row[0], role: "student")
      end

      success = Registration.transaction do
        if r.save && r.save_sections
          true
        else
          raise ActiveRecord::Rollback, "Saving registration failed"
        end
      end
      if success
        num_added += 1
      else
        failed << "#{row[0]} (#{r.errors.full_messages.to_sentence})"
      end
    end

    if failed.blank?
      redirect_to course_registrations_path(@course),
                  notice: "Added #{pluralize(num_added, 'student')}."
    else
      failed.each do |f| @course.errors.add(:base, f) end
      redirect_to course_registrations_path(@course),
                  notice: "Added #{pluralize(num_added, 'student')}.",
                  alert: "Could not add #{pluralize(failed.count, 'student')}: #{failed.to_sentence}"
    end
  end

  def destroy
    @registration.destroy

    redirect_to course_registrations_path(@course)
  end

  private

  def find_registration
    @registration = Registration.find_by(id: params[:id])
    if @registration.nil?
      redirect_back fallback_location: (@course ? course_registrations_path(@course) : root_path),
                    alert: "No such registration"
      return
    end
    @course = @registration.course
    @user   = @registration.user
  end

  def registration_params
    ans = params.require(:registration)
          .permit(:course_id, :orig_sections, :new_sections, :role, :username, :show_in_lists, :tags)
    ans[:course_id] = params[:course_id]
    if params[:new_sections]
      ans[:new_sections] = Section.where(crn: params[:new_sections].reject(&:blank?), course: params[:course_id])
    end
    if params[:orig_sections]
      ans[:orig_sections] = Section.where(crn: params[:orig_sections].reject(&:blank?), course: params[:course_id])
    end
    ans
  end

  def section_param_names
    Section::types.map{|t, _| "#{t}_section"}
  end
end
