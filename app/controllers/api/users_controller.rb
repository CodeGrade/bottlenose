module Api
  class UsersController < ApiController
    before_action :require_admin, except: [:me]

    def me
      render json: serialize_user(current_user)
    end

    def show
      u = User.find_by(username: params[:id])
      return head :not_found unless u

      render json: serialize_user(u)
    end
  end
end
