class FilesController < ApplicationController

  @@resources_true_dir = "lib/assets"

  def upload
    file_path = Upload.base_upload_dir.join(params[:path])
    if valid_path_param(params[:path]) && File.file?(file_path)
      disp = get_file_disposition(File.extname(params[:path]).downcase)
      mime = ApplicationHelper.mime_type(params[:path])
      
      send_file file_path.to_s,
                filename: File.basename(params[:path]),
                disposition: disp,
                type: mime
    else
      render 'errors/not_found', layout: 'errors', formats: [:html], status: 404
    end
  end

  def resource
    file_path = Rails.root.join(@@resources_true_dir, params[:path])
    if valid_path_param(params[:path]) && File.file?(file_path)
      disp = get_file_disposition(File.extname(params[:path]).downcase)
      mime = ApplicationHelper.mime_type(params[:path])

      send_file file_path.to_s,
                filename: File.basename(params[:path]),
                disposition: disp,
                type: mime
    else
      render 'errors/not_found', layout: 'errors', formats: [:html], status: 404
    end
  end


  private

  def valid_path_param(path)
    !path.include? "../"
  end
  
  def get_file_disposition(file_ext)
    case file_ext
    when ".jpg", ".jpeg", ".png", ".gif", ".tap", ".log"
      return "inline"
    when ".pdf"
      return nil
    else
      return "attachment"
    end
  end

end
