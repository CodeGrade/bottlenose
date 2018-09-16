require 'audit'
class ErrorsController < ApplicationController
  def not_found
    respond_to do |format|
      format.html { render status: 404 }
      format.png {
        if params[:any].starts_with? "apple-touch-icon"
          render body: nil, status: 410
        else
          render body: nil, status: 404
        end
      }
      format.js {
        Audit.log <<ERROR
Unknown <script> resource:  #{request.original_url}
Current user (in any):      #{current_user&.id} #{current_user&.display_name}
Remote address:             #{request.remote_ip}  
ERROR
        # The explicit content type prevents Rails from thinking this is a CSRF
        # attack and producing an incorrect error message
        render body: nil, status: 404, content_type: 'text/plain'
      }
        
      format.any {
        Audit.log <<ERROR
Unknown resource requested: #{request.original_url}
Unknown format:             #{params[:format]}
Current user (if any):      #{current_user&.id} #{current_user&.display_name}
Remote address:             #{request.remote_ip}  
ERROR
        render body: nil, status: 404
      }
    end
  end

  def internal_server_error
    render status: 500, formats: :html
  end
end
