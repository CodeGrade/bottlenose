class UsersController < ApplicationController
  before_action :require_site_admin, only: [:index]
  def index
    @users = User.order(:name)
    @user  = User.new
  end

  def show
    unless current_user_site_admin? || current_user_prof_ever? || current_user_has_id?(params[:id].to_i)
      redirect_to root_path, alert: "You don't have sufficient permission to see that information"
      return
    end

    find_user
  end

  def edit
    unless current_user_site_admin? || current_user_has_id?(params[:id].to_i)
      redirect_to root_path, alert: "You don't have sufficient permission to edit that information"
      return
    end

    find_user
  end

  def lookup
    u = User.find_by(username: params[:username])
    respond_to do |f|
      f.json {
        if u
          render :json => {name: u.name}
        else
          render :json => {name: "no one found"}, status: 400
        end
      }
    end
  end

  def update
    unless current_user_site_admin? || current_user_has_id?(params[:id].to_i)
      redirect_to root_path, alert: "You don't have sufficient information to update that information"
      return
    end

    find_user(courses_path)

    up = user_params

    if @user.profile && File.exists?(@user.profile) && up[:profile]
      FileUtils.rm(@user.profile)
      @user.profile = nil
    end
    if up[:profile]
      image = up[:profile]
      secret = SecureRandom.urlsafe_base64
      filename = Upload.base_upload_dir.join("#{secret}_#{@user.username}_profile#{File.extname(image.original_filename)}")
      File.open(filename, "wb") do |f| f.write(image.read) end
      up[:profile] = filename.to_s
    end
    
    if @user.update_attributes(up)
      if current_user_site_admin?
        redirect_to user_path(@user), notice: 'User was successfully updated.'
      else
        redirect_to '/courses', notice: "Profile successfully updated"
      end
    else
      FileUtils.rm(up[:profile]) if up[:profile] # cleanup orphaned file
      render action: "edit", 
             alert: "Error updating user: #{@user.errors.full_messages.join('; ')}", status: 400
    end
  end

  def impersonate
    unless current_user_site_admin? || current_user_prof_ever?
      redirect_to root_path, alert: "Must be an admin or professor"
      return
    end

    find_user(root_path)
    impersonate_user(@user)
    redirect_to root_path, notice: "You are impersonating #{@user.display_name}."
  end

  def stop_impersonating
    stop_impersonating_user
    redirect_to root_path, notice: "You are not impersonating anyone anymore."
  end

  private

  def user_params
    if current_user_site_admin?
      params[:user].permit(:email, :name, :nickname, :first_name, :last_name, :nuid, :profile, :site_admin)
    else
      params[:user].permit(:email, :name, :nickname, :first_name, :last_name, :nuid, :profile)
    end
  end

  def find_user(fallback_path = nil)
    fallback_path ||= users_path
    @user = User.find_by(id: params[:id])
    if @user.nil?
      redirect_back fallback_location: fallback_path, alert: "No such user"
      return
    end
  end
end
