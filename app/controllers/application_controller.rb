# coding: utf-8
class ApplicationController < ActionController::Base
  impersonates :user

  rescue_from DeviseLdapAuthenticatable::LdapException do |exception|
    render :text => exception, :status => 500
  end
  protect_from_forgery

  before_action :set_mailer_host
  before_action :configure_permitted_parameters, if: :devise_controller?

  protected

  def multi_group_by(hash, keys, index = 0)
    if index >= keys.count || hash.nil?
      hash
    else
      Hash[hash.group_by(&(keys[index])).map {|k, v| [k, multi_group_by(v, keys, index + 1)]}]
    end
  end

  def set_mailer_host
    ActionMailer::Base.default_url_options[:host] = request.host_with_port
    ActionMailer::Base.default_url_options[:protocol] = request.protocol

    if Settings["site_url"].blank?
      Settings.set_site_url!(request)
    end
  end

  def get_queue_info
    begin
      bean = Beaneater.new("localhost:11300")
      tube = bean.tubes["bottlenose.#{Rails.env}.backburner-jobs"]
      @queue_stats = tube.stats
    rescue
      @queue_stats = nil
    end
  end

  def require_site_admin
    unless current_user_site_admin?
      redirect_to root_path, alert: "Must be an admin."
      return
    end
  end

  # Require that there is a `current_user` indicating that a user is currently
  # logged in.
  def require_current_user
    if current_user.nil?
      redirect_to root_path, alert: "You need to log in first."
      return
    end
  end

  def current_user_site_admin?
    current_user&.site_admin?
  end

  def current_user_prof_ever?
    current_user&.professor_ever?
  end

  def current_user_prof_for?(course)
    return false if course.nil?
    current_user && (current_user.site_admin? || current_user.registration_for(course).professor?)
  end

  def current_user_staff_for?(course)
    current_user && (current_user.site_admin? || current_user.registration_for(course).staff?)
  end

  def true_user_prof_for?(course)
    true_user && (true_user.site_admin? || true_user.registration_for(course).professor?)
  end

  def true_user_staff_for?(course)
    true_user && (true_user.site_admin? || true_user.registration_for(course).staff?)
  end

  def current_user_has_id?(id)
    current_user&.id == id
  end

  def configure_permitted_parameters
    devise_parameter_sanitizer.permit(:sign_up) do |user|
      user.permit(:name, :email, :username, :password, :password_confirmation)
    end
  end

  def array_from_hash(h)
    return h unless h.is_a? Hash

    all_numbers = h.keys.all? { |k| k.to_i.to_s == k }
    if all_numbers
      ans = []
      h.keys.sort_by{ |k| k.to_i }.map{ |i| ans[i.to_i] = array_from_hash(h[i]) }
      ans
    else
      ans = {}
      h.each do |k, v|
        ans[k] = array_from_hash(v)
      end
      ans
    end
  end
  
  # Course stuff

  def find_course
    return unless @course.nil?

    @course = Course.find_by(id: params[:course_id] || params[:id])
    if @course.nil?
      redirect_to courses_path, alert: "No such course"
      return
    end
  end

  def require_registered_user
    require_current_user
    return if current_user.nil?
    
    find_course

    return if current_user_site_admin?

    @registration = current_user.registration_for(@course)
    if @registration.nil?
      redirect_to courses_path, alert: "You're not registered for that course."
      return
    elsif @registration.dropped_date
      redirect_to courses_path, alert: "You've already dropped that course."
      return
    end
  end

  def require_admin_or_prof
    unless current_user_site_admin? || current_user_prof_for?(@course)
      redirect_back fallback_location: root_path, alert: "Must be an admin or professor."
      return
    end
  end

  def require_admin_or_staff
    unless current_user_site_admin? || current_user_staff_for?(@course)
      redirect_back fallback_location: root_path, alert: "Must be an admin or staff."
      return
    end
  end

  def pluralize(count, word, plural = nil)
    ActionController::Base.helpers.pluralize(count, word, plural)
  end
end
