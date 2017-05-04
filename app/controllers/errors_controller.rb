class ErrorsController < ApplicationController
  def not_found
    respond_to do |format|
      format.html { render status: 404 }
    end
  rescue ActionController::UnknownFormat
    render status: 404, text: "Nothing found"
  end

  def internal_server_error
    render(:status => 500)
  end
end
