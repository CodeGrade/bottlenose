module Api
  class CoursesController < ApiController
    def show
      render json: {
        this: "show",
        you: current_api_user.username,
        course: Course.find(params[:id]).name
      }
    end

    def index
      render json: { this: "index", you: current_api_user.username }
    end
  end
end
