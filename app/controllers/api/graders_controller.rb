module Api
  class GradersController < ApiController
    skip_before_action :doorkeeper_authorize!
    before_action :find_grader

    def orca_response
      return head :missing if @grader.nil?
      return head :bad_request unless @grader.orca_status

      File.open(@grader.orca_build_result_path, 'w') do |f|
        f.write(JSON.generate(orca_build_result_body))
      end
      head :ok
    end

    private

    def find_grader
      @grader = Grader.find_by_id params[:id]
    end

    def orca_build_result_body
      {
        was_successful: params[:was_successful],
        logs: params[:logs].map { |l| l.permit!.to_h }
      }
    end
  end
end
