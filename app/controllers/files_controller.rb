class FilesController < ApplicationController

  @@resources_true_dir = "lib/assets"

  def upload
    if File.file?(Upload.base_upload_dir.join(params[:path]))
      disp = get_file_disposition(File.extname(params[:path]).downcase)
      mime = ApplicationHelper.mime_type(params[:path])
      
      send_file Upload.base_upload_dir.join(params[:path]).to_s,
                filename: File.basename(params[:path]),
                disposition: disp,
                type: mime
    else
      render 'errors/not_found', layout: 'errors', formats: [:html], status: 404
    end
  end

  def resource
    if File.file?(Pathname.new(@@resources_true_dir).join(params[:path]))
      disp = get_file_disposition(File.extname(params[:path]).downcase)
      mime = ApplicationHelper.mime_type(params[:path])

      send_file Pathname.new(@@resources_true_dir).join(params[:path]).to_s,
                filename: File.basename(params[:path]),
                disposition: disp,
                type: mime
    else
      render 'errors/not_found', layout: 'errors', formats: [:html], status: 404
    end
  end


  private
  
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
