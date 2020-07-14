module Oauth
  class ApplicationsController < Doorkeeper::ApplicationsController
    # Allow an admin to impersonate anyone to audit their OAuth apps
    impersonates :user

    def index
      @applications =
        if current_user.site_admin?
          Doorkeeper::Application.all
        else
          current_user.oauth_applications
        end
    end

    def create
      @application = Doorkeeper::Application.new(application_params)
      @application.owner = current_user if Doorkeeper.configuration.confirm_application_owner?
      if @application.save
        flash[:notice] = I18n.t(:notice, :scope => [:doorkeeper, :flash, :applications, :create])
        redirect_to oauth_application_url(@application)
      else
        render :new
      end
    end

    private

    def set_application
      @application = current_user.oauth_applications.find(params[:id])
    end
  end
end
