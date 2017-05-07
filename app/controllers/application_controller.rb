class ApplicationController < ActionController::Base
  impersonates :user

  rescue_from DeviseLdapAuthenticatable::LdapException do |exception|
    render :text => exception, :status => 500
  end
  protect_from_forgery

  before_action :set_mailer_host
  before_action :configure_permitted_parameters, if: :devise_controller?

  protected

  def set_mailer_host
    ActionMailer::Base.default_url_options[:host] = request.host_with_port
    ActionMailer::Base.default_url_options[:protocol] = request.protocol
  end

  def find_course
    return unless @course.nil?

    if params[:course_id].nil?
      @course ||= Course.find(params[:id])
    else
      @course ||= Course.find(params[:course_id])
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

  def require_valid_course
    find_course

    if @course.nil?
      redirect_to back_or_else(courses_path), alert: "No such course"
      return
    end
  end

  def current_user_site_admin?
    current_user && current_user.site_admin?
  end

  def current_user_prof_ever?
    current_user && current_user.professor_ever?
  end

  def current_user_prof_for?(course)
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
    current_user && current_user.id == id
  end

  def configure_permitted_parameters
    devise_parameter_sanitizer.permit(:sign_up) do |user|
      user.permit(:name, :email, :username, :password, :password_confirmation)
    end
  end

  def back_or_else(target)
    if request.env["HTTP_REFERER"].present? and request.env["HTTP_REFERER"] != request.env["REQUEST_URI"]
      :back
    else
      target
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


  def get_submission_files(sub, line_comments = nil, show_deductions = false)
    show_hidden = (current_user_site_admin? || current_user_staff_for?(@course))
    @lineCommentsByFile = line_comments || sub.grade_line_comments(nil, show_hidden)
    @submission_files = []
    @show_deductions = show_deductions
    def with_extracted(item)
      return nil if item.nil?
      if item[:public_link]
        return nil if File.basename(item[:full_path].to_s) == ".DS_Store"
        comments = @lineCommentsByFile[item[:public_link].to_s] || {noCommentsFor: item[:public_link].to_s}
        @submission_files.push({
          link: item[:public_link],
          name: item[:public_link].sub(/^.*extracted\//, ""),
          contents: File.read(item[:full_path].to_s),
          type: case File.extname(item[:full_path].to_s).downcase
                when ".java"
                  "text/x-java"
                when ".js"
                  "text/javascript"
                when ".arr"
                  "pyret"
                when ".rkt", ".ss"
                  "scheme"
                when ".ml", ".mli"
                  "mllike"
                when ".mly"
                  "text/x-ebnf"
                when ".c", ".h"
                  "text/x-csrc"
                when ".cpp", ".c++"
                  "text/x-c++src"
                when ".cs"
                  "text/x-csharp"
                when ".jpg", ".jpeg", ".png"
                  "image"
                when ".jar"
                  "jar"
                when ".zip"
                  "zip"
                else
                  if File.basename(item[:full_path].to_s) == "Makefile"
                    "text/x-makefile"
                  else
                    "text/unknown"
                  end
                end,
          href: @submission_files.count + 1,
          lineComments: comments
          })
        deductions =
          if comments[:noCommentsFor]
            nil
          elsif @show_deductions
            comments.reduce(nil) do |sum, (type, commentsByType)|
              if commentsByType.is_a? String
                sum
              elsif @show_deductions.is_a? String and @show_deductions != type
                sum
              else
                commentsByType.reduce(sum) do |sum, (line, comments)|
                  comments.reduce(sum) do |sum, comment|
                    (sum || 0) - comment[:deduction]
                  end
                end
              end
            end
          end
        { text:
            if deductions
              "#{item[:path]} (#{deductions})"
            else
              item[:path]
            end,
          href: @submission_files.count,
          #icon: @lineCommentsByFile[item[:public_link].to_s] ? "glyphicon glyphicon-flash" : ""
        }
      else
        return nil if item[:path] == "__MACOSX"
        {
          text: item[:path] + "/",
          state: {selectable: true},
          nodes: item[:children].map{|i| with_extracted(i)}.compact
        }
      end
    end

    @submission_dirs = sub.upload.extracted_files.map{|i| with_extracted(i)}.compact

    @count = @submission_files.count.to_s.length

    def fix_hrefs(node)
      if node[:href].is_a? Integer
        node[:href] = "#file_" + node[:href].to_s.rjust(@count, '0')
      end
      if node[:nodes]
        node[:nodes].each do |n| fix_hrefs(n) end
      end
    end
    fix_hrefs({nodes: @submission_dirs})
    fix_hrefs({nodes: @submission_files})
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
    find_course

    unless current_user_site_admin? || current_user_prof_for?(@course)
      redirect_to back_or_else(root_path), alert: "Must be an admin or professor."
      return
    end
  end

  def require_admin_or_staff
    unless current_user_site_admin? || current_user_staff_for?(@course)
      redirect_to back_or_else(root_path), alert: "Must be an admin or staff."
      return
    end
  end
end
