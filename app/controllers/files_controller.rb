class FilesController < ApplicationController
  def upload
    if File.file?(Upload.base_upload_dir.join(params[:path]))
      case File.extname(params[:path]).downcase
      when ".jpg", ".jpeg", ".png", ".gif"
        disp = "inline"
      else
        disp = "attachment"
      end
      mime = mime_type(params[:path])
      
      send_file Upload.base_upload_dir.join(params[:path]).to_s,
                filename: File.basename(params[:path]),
                disposition: disp,
                type: mime
    else
      render 'errors/not_found', layout: 'errors', status: 404
    end
  end
end
