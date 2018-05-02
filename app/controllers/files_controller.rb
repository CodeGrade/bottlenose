class FilesController < ApplicationController
  def upload
    file_path = Upload.base_upload_dir.join(params[:path])
    unless File.file?(file_path)
      file_path = Rails.root.join("private", params[:path])
    end

    if File.file?(file_path)
      case File.extname(params[:path]).downcase
      when ".jpg", ".jpeg", ".png", ".gif", ".tap"
        disp = "inline"
      else
        disp = "attachment"
      end
      mime = ApplicationHelper.mime_type(params[:path])
      
      send_file file_path.to_s,
                filename: File.basename(params[:path]),
                disposition: disp,
                type: mime
    else
      render 'errors/not_found', layout: 'errors', formats: [:html], status: 404
    end
  end
end
