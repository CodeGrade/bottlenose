module Api
  class CredentialsController < ApiController
    def me
      u = current_api_user
      render json: {
        username: u.username,
        display_name: u.display_name,
        nuid: u.nuid,
        email: u.email,
        prof: u.professor_ever?,
        image_url: u.profile
      }
    end
  end
end
