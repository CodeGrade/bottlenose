class ApiController < ApplicationController
  before_action :doorkeeper_authorize!
  respond_to    :json

  def require_admin
    head :forbidden unless current_api_user&.site_admin?
  end

  def current_api_user
    User.find(doorkeeper_token.resource_owner_id) if doorkeeper_token
  end
end
