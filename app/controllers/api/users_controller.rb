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

    private

    def serialize_user(user)
      {
        username: user.username,
        display_name: user.display_name,
        nuid: user.nuid,
        email: user.email,
        prof: user.professor_ever?,
        image_url: user.profile
      }
    end
  end
end
