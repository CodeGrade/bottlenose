class ErrorsController < ApplicationController
  def not_found
    respond_to do |format|
      format.html { render status: 404 }
      format.png {
        if params[:any].starts_with "apple-touch-icon"
          render body: nil, status: 410
        else
          render body: nil, status: 404
        end
      }
    end
  end

  def internal_server_error
    render(:status => 500)
  end
end
