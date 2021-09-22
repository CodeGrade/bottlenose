class ApiController < ApplicationController
  before_action :doorkeeper_authorize!
  respond_to    :json

  def find_course(id = nil)
    return unless @course.nil?

    @course = Course.find_by(id: id || params[:course_id])
    head :not_found if @course.nil?
  end

  def require_admin_or_prof
    admin_or_prof = current_user_site_admin? || current_user_prof_for?(@course)
    head :forbidden unless admin_or_prof
  end

  def require_admin
    head :forbidden unless current_user_site_admin?
  end

  def require_admin_or_prof_ever
    admin_or_prof = current_user_site_admin? || current_user&.professor_ever?
    head :forbidden unless admin_or_prof
  end

  def current_user
    User.find(doorkeeper_token.resource_owner_id) if doorkeeper_token
  end

  def require_registered_user
    find_course
    return if current_user_site_admin?

    @registration = current_user.registration_for(@course)
    if @registration.nil?
      head :forbidden
    elsif @registration.dropped_date
      head :forbidden
    end
  end

  private

  def serialize_user(user)
    {
      username: user.username,
      display_name: user.display_name,
      nuid: user.nuid,
      email: user.email,
      image_url: Upload.upload_path_for(user.profile)
    }
  end
end
