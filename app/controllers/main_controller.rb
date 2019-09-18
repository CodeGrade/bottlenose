require 'junit_grader'

class MainController < ApplicationController
  before_action :require_site_admin, only: [:clear_queue]
  before_action :require_admin_or_prof, only: [:status]
  
  # GET /
  def home
    if current_user
      if (current_user.sign_in_count == 1 && (current_user.profile.to_s.empty? || current_user.nickname.to_s.empty?))
        redirect_to edit_user_path(current_user), notice: profile_notice
      elsif params[:next]
        redirect_to params[:next]
        return
      else
        @teams = multi_group_by(current_user.teams.includes(:users).order(end_date: :desc, id: :asc),
                                [:course_id, :teamset_id])
        render "dashboard"
      end
    else
      redirect_to new_user_session_path(next: params[:next])
    end
  end

  def resource_name
    :user
  end
  helper_method :resource_name

  def resource
    @resource ||= User.new
  end
  helper_method :resource

  def devise_mapping
    @devise_mapping ||= Devise.mappings[:user]
  end
  helper_method :devise_mapping

  # GET /about
  def about
  end

  def status
    if current_user.nil?
      redirect_to new_user_session_path
      return
    end

    get_queue_info
  end

  def clear_queue
    cleared = Grader::GradingJob.clear_all!
    if cleared.is_a? String
      redirect_to server_status_path, alert: cleared
    else
      redirect_to server_status_path, notice: "#{pluralize(cleared, 'job')} cleared"
    end
  end

  protected

  def profile_notice
    <<NOTICE.html_safe
Please complete your user profile, so we can recognize you in class: 
<ul>
<li>Please give us a <i>recognizable</i> profile picture</li>
<li>Please fill in your preferred nickname</li>
<li>Please make sure you can receive email at the specified address</li>
</ul>
NOTICE
  end
end
