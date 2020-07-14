module Api
  class UsersController < ApiController
    def me
      render json: serialize_user(current_user)
    end
  end
end
