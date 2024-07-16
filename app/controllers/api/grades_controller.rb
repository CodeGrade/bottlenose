module Api
  class GradesController < ApiController
    skip_before_action :doorkeeper_authorize!

    def orca_response
      body = orca_response_params
      key = body[:key]
      grade_id = key['grade_id']
      secret = key['secret']
      grade = Grade.find_by(id: grade_id)
      if grade.nil?
        return render json: { message: "No grade for ID #{grade_id}" },
                      status: :not_found
      end
      grader_dir = grade.submission_grader_dir
      secret_file_path = grader_dir.join('orca.secret')
      unless valid_orca_response?(secret_file_path, secret)
        return render json: { message: 'Invalid secret provided.' },
                      status: :not_found
      end
      File.open(grader_dir.join('result.json'), 'w') do |f|
        f.write(JSON.generate(body.except(:key)))
      end
      FileUtils.rm(secret_file_path)
      render json: { message: 'OK' }, status: :ok
    end

    private

    def valid_orca_response?(secret_file_path, response_secret)
      return false unless File.exist? secret_file_path

      File.open(secret_file_path) do |fp|
        secret_from_file = fp.read
        return secret_from_file == response_secret
      end
    end

    def orca_response_params
      key = params.require(:key)
      {
        key: JSON.parse(key),
        shell_responses: params[:shell_responses] || [],
        output: params[:output] || [],
        errors: params[:errors] || []
      }
    end
  end
end
