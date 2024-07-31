module Api
  class GradesController < ApiController
    skip_before_action :doorkeeper_authorize!
    before_action :find_grade

    def orca_response
      response_params = orca_response_params
      id_update_params = orca_id_update_params
      return head :bad_request if response_params.nil? && id_update_params.nil?

      response_params.nil? ? handle_orca_id_update(id_update_params) : handle_response(response_params)
    end

    private

    def handle_response(response)
      key = response[:key]
      secret = key['secret']
      grader = @grade.grader
      return head :bad_request unless grader.valid_orca_secret? secret, @grade

      File.open(@grade.submission_grader_dir.join('result.json'), 'w') do |f|
        f.write(JSON.generate(response.except(:key)))
      end

      FileUtils.rm(grader.orca_secret_path(@grade))
      FileUtils.rm(grader.orca_id_path(@grade))
      head :ok
    end

    def handle_orca_id_update(response)
      new_id = response[:job_id]
      @grade.grader.save_orca_id @grade, new_id
      head :ok
    end

    def orca_response_params
      key = params.require(:key)
      shell_responses = (params[:shell_responses] || []).map do |response_as_param|
        shell_response_hash = response_as_param.permit!.to_h
        {
          **shell_response_hash,
          status_code: shell_response_hash[:status_code].to_i,
          timed_out: shell_response_hash[:timed_out] == 'true'
        }
      end
      errors = params[:errors] || []
      output = params[:output]
      {
        key: JSON.parse(key, { max_nesting: 1, create_additons: false }),
        shell_responses: shell_responses,
        output: output,
        errors: errors
      }
    rescue ActionController::ParameterMissing
      nil
    end

    def orca_id_update_params
      {
        job_id: params.require(:jobID).to_i
      }
    rescue ActionController::ParameterMissing
      nil
    end

    def find_grade
      @grade = Grade.find_by(id: params[:id])
      head :bad_request if @grade.nil?
    end
  end
end
