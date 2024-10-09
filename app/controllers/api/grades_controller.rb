require 'audit'

module Api
  class GradesController < ApiController
    skip_before_action :doorkeeper_authorize!
    before_action :find_grade

    def orca_response
      config_details = "(grade #{@grade.id}, grader #{@grader.id})"
      Audit.log "In GradesController#{config_details}.orca_response(#{params})\n"
      response_params = orca_response_params
      update_params = orca_job_status_update_params
      return head :bad_request if response_params.nil? && update_params.nil?

      secret = (response_params.nil? ? update_params : response_params)[:key]['secret']
      return head :bad_request unless @grader.valid_orca_secret? secret, @grade

      response_params.nil? ? handle_job_status_update(update_params) : handle_response(response_params)
    end

    private

    def cleanup(node, replace)
      case node
      when String
        replace.each do |s, r|
          node = node.gsub(s, r)
        end
        node
      when Array
        node.map { |item| cleanup(item, replace) }
      when Hash
        node.to_h { |k, v| [cleanup(k, replace), cleanup(v, replace)] }
      else
        node
      end
    end
    
    def handle_response(response)
      config_details = "(grade #{@grade.id}, grader #{@grader.id})"
      replacements = {
        "$EXTRACTED/submission" => Upload.upload_path_for(@grade.submission.upload.extracted_path.to_s)
      } 
      response = cleanup(response, replacements)
      
      File.open(@grade.orca_result_path, 'w') do |f|
        f.write(JSON.generate(response.except(:key)))
      end

      FileUtils.rm(@grader.orca_secret_path(@grade))
      FileUtils.rm(@grader.orca_job_status_path(@grade))
      begin
        @grader.postprocess_orca_response(@grade, response.except(:key))
      rescue Exception => e
        Audit.log "In GradesController#{config_details}.orca_response: Error postprocessing response: #{e}\n#{e.backtrace.join('\n')}\n"
        Audit.log "In GradesController#{config_details}.orca_response: Response was: #{response}\n"
      end
      head :ok
    end

    def handle_job_status_update(response)
      @grader.save_orca_job_status @grade, response[:status]
      head :ok
    end

    def orca_response_params
      begin
        key = params.require(:key)
        if params[:shell_responses].nil? && params[:errors].nil?
          raise ActionController::ParameterMissing,
                'Orca response must have either shell_responses or errors.'
        end
        shell_responses = (params[:shell_responses] || []).map do |response_as_param|
          shell_response_hash = response_as_param.permit!.to_h
          {
            **shell_response_hash,
            status_code: shell_response_hash[:status_code].to_i,
            did_timeout: shell_response_hash[:did_timeout] == 'true'
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
    end

    def orca_job_status_update_params
      begin
        key = JSON.parse(params.require(:key))
        {
          key: key,
          status: params.require(:status).permit!.to_h
        }
      rescue ActionController::ParameterMissing
        nil
      end
    end

    def find_grade
      @grade = Grade.find_by(id: params[:id])
      return head :bad_request if @grade.nil?

      @grader = @grade.grader
    end
  end
end
