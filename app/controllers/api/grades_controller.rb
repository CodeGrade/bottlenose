module Api
  class GradesController < ApiController
    skip_before_action :doorkeeper_authorize!

    def orca_response
      body = orca_response_params
      key = body[:key]
      grade_id = key['grade_id']
      secret = key['secret']
      grade = Grade.find_by(id: grade_id)
      return head :bad_request if grade.nil?

      grader = grade.grader
      return head :bad_request  unless grader.valid_orca_secret? secret, grade

      File.open(grade.submission_grader_dir.join('result.json'), 'w') do |f|
        f.write(JSON.generate(body.except(:key)))
      end

      FileUtils.rm(grader.orca_secret_path(grade))
      head :ok
    end

    private

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
    end
  end
end