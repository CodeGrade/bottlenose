require 'csv'

class RegistrationsController < CoursesController
  prepend_before_action :find_registration, except: [:index, :new, :create, :bulk_enter, :bulk_update, :bulk_edit]
  before_action :require_admin_or_staff
  
  def index
    @students = @course.students
    @staff = @course.staff
    @requests = @course.reg_requests.joins(:user).order('role desc', 'name').includes(:user)
  end

  def new
    @registration = Registration.new
  end

  def create
    reg_params = registration_params
    # Create @registration object for errors.
    @registration = Registration.new()

    if params[:username].blank?
      @registration.errors[:base] << "Must provide a username."
      render action: "new"
      return
    end

    # Set the @registration to the new registration on @course.
    @registration = @course.add_registration(params[:username],
                                             reg_params[:section].to_i,
                                             RegRequest::roles[reg_params[:role]])
    if @registration and @registration.save
      redirect_to course_registrations_path(@course),
                  notice: 'Registration was successfully created.'
    else

      render action: :new
    end
  end

  def bulk_edit
    @course = Course.find(params[:course_id])
    if params[:role] == "student"
      @registrations = @course.registrations
                       .where(role: Registration::roles["student"])
    else
      @registrations = @course.registrations
                       .where.not(role: Registration::roles["student"])
    end
    @registrations = @registrations
                     .includes(:user)
                     .includes(:section)
                     .to_a.sort_by{|r| r.user.display_name}
  end

  def bulk_update
    respond_to do |f|
      f.json {
        @reg = Registration.find(params[:id])
        if @reg.nil? or @reg.course.id != @course.id
          render :json => {failure: "Unknown registration"}
        else
          if @reg.role != params[:role] or
             params[:section].to_i != @reg.section_id or
             (params[:reenroll] and !@reg.dropped_date.nil?)
            @reg.section_id = params[:section].to_i
            @reg.dropped_date = nil if params[:reenroll]
            @reg.role = params[:role]
            @reg.save
            render :json => @reg
          else
            render :json => {"no-change": true}
          end
        end
      }
      f.html { redirect_to back_or_else(course_registrations_path(@course)),
                           notice: "No such page" }
    end
  end

  def bulk_enter
    @course = Course.find(params[:course_id])
    num_added = 0

    CSV.parse(params[:usernames]) do |row|
      if @course.add_registration(row[0], row[1])
        num_added += 1
      end
    end

    redirect_to course_registrations_path(@course),
                notice: "Added #{num_added} students."
  end

  def destroy
    @registration.destroy

    redirect_to course_registrations_path(@course)
  end

  def toggle
    @registration.show_in_lists = ! @registration.show_in_lists?
    @registration.save

    @show = @registration.show_in_lists? ? "Yes" : "No"

    redirect_to back_or_else(course_registrations_path(@course))
  end

  private

  def find_registration
    @registration = Registration.find(params[:id])
    @course = @registration.course
    @user   = @registration.user
  end

  def registration_params
    params.require(:registration)
          .permit(:course_id, :section, :role, :user_id, :show_in_lists, :tags)
  end

  def require_admin_or_staff
    unless current_user_site_admin? || current_user_staff_for?(@course)
      redirect_to back_or_else(root_path), alert: "Must be an admin or staff."
      return
    end
  end
end
