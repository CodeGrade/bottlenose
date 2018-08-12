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

  def multi_group_by(hash, keys, last_key_unique = false, index = 0)
    if index >= keys.count || hash.nil?
      hash
    elsif index == keys.count - 1 && last_key_unique
      Hash[hash.map{|v| [(v[keys[index]] rescue v.__send__(keys[index])), v]}]
    else
      Hash[hash.group_by(&(keys[index])).map {|k, v| [k, multi_group_by(v, keys, last_key_unique, index + 1)]}]
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
    Grader::GradingJob.prune(0)
    @queue_stats = Grader.delayed_grades
    now = Time.now
    @avg_wait = @queue_stats.reduce(0) do |sum, (k, v)|
      delay = now - v[:start_time]
      v[:wait_s] = "#{(delay / 60).to_i} minutes, #{(delay % 60).to_i} seconds"
      sum + delay
    end
    if @queue_stats.length != 0
      @avg_wait = @avg_wait / @queue_stats.length
    end
    @avg_wait_msg = "#{(@avg_wait / 60).to_i} minutes, #{(@avg_wait % 60).to_i} seconds"
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
    current_user && (current_user.site_admin? || current_user.registration_for(course)&.professor?)
  end

  def current_user_assistant_for?(course)
    return false if course.nil?
    current_user && (current_user.site_admin? || current_user.registration_for(course)&.assistant?)
  end

  def current_user_staff_for?(course)
    current_user && (current_user.site_admin? || current_user.registration_for(course)&.staff?)
  end

  def true_user_prof_for?(course)
    true_user && (true_user.site_admin? || true_user.registration_for(course)&.professor?)
  end

  def true_user_staff_for?(course)
    true_user && (true_user.site_admin? || true_user.registration_for(course)&.staff?)
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

  def find_course(id = nil)
    return unless @course.nil?

    @course = Course.find_by(id: id || params[:course_id])
    if @course.nil?
      redirect_to courses_path, alert: "No such course"
      return
    end
  end

  def find_assignment(id = nil)
    return unless @assignment.nil?

    @assignment = Assignment.find_by(id: id || params[:assignment_id])
    if @assignment.nil? || (@assignment.course_id != @course.id)
      redirect_back fallback_location: course_path(@course), alert: "No such assignment for this course"
      return
    end
  end

  def find_submission(id = nil)
    @submission = Submission.find_by(id: id || params[:submission_id])
    if @submission.nil?
      redirect_back fallback_location: course_assignment_path(@course, @assignment),
                    alert: "No such submission"
      return
    end
    if @submission.assignment_id != @assignment.id
      redirect_back fallback_location: course_assignment_path(@course, @assignment), alert: "No such submission for this assignment"
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

  def require_admin_or_prof(fallback_path = nil)
    fallback_path ||= root_path
    unless current_user_site_admin? || current_user_prof_for?(@course)
      redirect_back fallback_location: fallback_path, alert: "Must be an admin or professor."
      return
    end
  end

  def require_admin_or_prof_ever(fallback_path = nil)
    fallback_path ||= root_path
    unless current_user_site_admin? || current_user&.professor_ever?
      redirect_back fallback_location: fallback_path, alert: "Must be an admin or a professor."
      return
    end
  end

  def require_admin_or_assistant(fallback_path = nil)
    fallback_path ||= root_path
    unless current_user_site_admin? || current_user_assistant_for?(@course)
      redirect_back fallback_location: fallback_path, alert: "Must be an admin, professor or assistant."
      return
    end
  end

  def require_admin_or_staff(fallback_path = nil)
    fallback_path ||= root_path
    unless current_user_site_admin? || current_user_staff_for?(@course)
      redirect_back fallback_location: fallback_path, alert: "Must be an admin or staff."
      return
    end
  end

  def pluralize(count, word, plural = nil)
    ActionController::Base.helpers.pluralize(count, word, plural)
  end
end
