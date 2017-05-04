require 'junit_grader'

class MainController < ApplicationController
  # GET /
  def home
    if current_user
      if (current_user.sign_in_count == 1 and (current_user.profile.to_s.empty? or current_user.nickname.to_s.empty?))
        redirect_to edit_user_path(current_user), notice: profile_notice
      else
        render "dashboard"
      end
    else
      render "landing"
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
      render "landing"
      return
    end
    backlog = Delayed::Job.all.to_a
    now = DateTime.now

    @backlog = backlog.map do |b|
      job_info = b.payload_object
      if job_info.is_a? Delayed::PerformableMethod
        w = now.to_time - b.created_at.to_time
        {
          sub: job_info.object,
          job_start: b.created_at,
          wait: w,
          wait_s: "#{(w / 60).to_i} minutes, #{(w % 60).to_i} seconds",
          method: job_info.method_name,
          args: job_info.args
        }
      else
        nil
      end
    end.reject(&:nil?)
    waits = @backlog.reduce(0) do |acc, b| acc + b[:wait] end
    if @backlog.length == 0
      avg_wait = 0
    else
      avg_wait = waits / @backlog.length
    end
    @avg_wait = "#{(avg_wait / 60).to_i} minutes, #{(avg_wait % 60).to_i} seconds"
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
