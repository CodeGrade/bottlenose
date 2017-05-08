class RegRequestsController < CoursesController
  skip_before_action :load_and_verify_course_registration

  # GET /courses/:course_id/reg_requests/new
  def new
    @course = Course.find(params[:course_id])
    @reg_request = @course.reg_requests.new
  end

  # POST /courses/:course_id/reg_requests
  def create
    @course = Course.find(params[:course_id])
    @reg_request = @course.reg_requests.new(reg_request_params)
    @reg_request.user = current_user

    if @reg_request.save
      @course.professors.each do |teacher|
        NotificationMailer.got_reg_request(teacher,
          @reg_request, root_url).deliver_later
      end

      redirect_to courses_path, notice: 'A registration request will be sent.'
    else
      render :new
    end
  end

  def accept
    @request = RegRequest.find(params[:id])
    @course = Course.find(params[:course_id])
    errs = accept_help(@request)
    if errs
      redirect_back fallback_location: course_registrations_path(@course), alert: errs
    else
      redirect_back fallback_location: course_registrations_path(@course)
    end
  end

  def accept_all
    count = 0
    @course = Course.find(params[:course_id])
    RegRequest.where(course_id: params[:course_id]).each do |req|
      errs = accept_help(req)
      if errs
        redirect_back fallback_location: course_registrations_path(@course), alert: errs
        return
      end
      count = count + 1
    end
    redirect_back fallback_location: course_registrations_path(@course), notice: "#{plural(count, 'registration')} added"
  end

  def accept_help(request)
    begin
      reg = Registration.find_or_create_by(user: request.user,
                                           course: request.course,
                                           role: request.role,
                                           section: request.section)
      if reg
        reg.show_in_lists = (request[:role] == RegRequest::roles[:student])
        reg.dropped_date = nil
        reg.save!
        request.destroy
      end
      nil
    rescue Exception => err
      err.record.errors
    end
  end
  

  def reject
    RegRequest.find(params[:id]).delete
    @course = Course.find(params[:course_id])
    redirect_back fallback_location: course_registrations_path(@course)
  end

  def reject_all
    RegRequest.where(course_id: params[:course_id]).delete_all # No dependents, so deletion is fine
    @course = Course.find(params[:course_id])
    redirect_back fallback_location: course_registrations_path(@course)
  end

  private

  def reg_request_params
    ans = params[:reg_request].permit(:notes, :role, :section)
    ans[:section] = Section.find_by(crn: ans[:section].to_i)
    ans
  end
end
