module Api
  class AssignmentsController < ApiController
    before_action :find_course
    before_action :require_admin_or_prof

    def create_or_update
      body = params.permit!.to_h
      summary = body[:exam_summary]
      grades = body[:exam_grades]
      finish_datetime = DateTime.parse(params[:finish_time])

      if body[:exam_id]
        @exam = Exam.find(body[:exam_id])
        unless @exam
          return render json: {
            errors: 'No such exam.'
          }, status: :bad_request
        end
        if @exam.course != @course
          return render json: {
            errors: 'No such exam in that course.'
          }, status: :bad_request
        end
      else
        @exam = Exam.new(
          blame: current_user,
          current_user: current_user,
          course: @course,
          due_date: finish_datetime,
          available: finish_datetime,
          lateness_config_id: @course.lateness_config_id,
          points_available: 0,
          request_time_taken: false
        )

        grader = ExamGrader.new(order: 1, upload_by_user_id: current_user.id)
        @exam.graders = [grader]
        @exam.teamset_plan = 'none'
      end

      @exam.name = body[:name]

      Tempfile.open('exam.yaml', Rails.root.join('tmp')) do |f|
        f.write(summary.to_yaml)
        f.flush
        f.rewind
        uploadfile = ActionDispatch::Http::UploadedFile.new(
          filename: 'exam.yaml',
          tempfile: f
        )
        @exam.assignment_file = uploadfile

        saved = @exam.save
        unless saved
          return render json: {
            errors: @exam.errors.full_messages.to_sentence
          }, status: :bad_request
        end
      end
      render json: {
        id: @exam.id
      }
    end
  end
end
