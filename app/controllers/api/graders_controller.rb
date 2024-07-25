module Api
  class GradersController < ApiController
    skip_before_action :doorkeeper_authorize!

    def orca_response
      begin
        body = orca_response_params
      rescue JSON::NestingError
        return head :bad_request
      end
      grader_id = body[:build_key]['grader_id']
      grader = Grader.find_by_id grader_id
      return head :bad_request if grader.nil?
      return head :bad_request unless grader.status['useOrca']

      # TODO: Write this to the build_result.json file for the given grader.
      build_status = body[:was_successful] ? 'Completed' : 'Failed'
      logs = body[:logs]
      grader.status = { **grader.status, 'logs' => logs, 'buildStatus' => build_status }
      grader.save!
      head :ok
    end

    private

    def orca_response_params
      build_key = JSON.parse(params.require(:build_key), {
                               max_nesting: 1,
                               create_additions: false
                             })
      {
        build_key: build_key,
        was_successful: params[:was_successful],
        logs: params[:logs].map { |l| l.permit!.to_h }
      }
    end
  end
end
