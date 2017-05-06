class AssignmentsController < CoursesController
  prepend_before_action :find_assignment, except: [:index, :new, :create, :edit_weights, :update_weights]
  before_action :require_valid_course
  before_action :require_admin_or_prof, only: [:edit, :edit_weights, :update, :update_weights,
                                               :new, :create, :destroy,
                                               :recreate_grades]
  before_action :require_admin_or_staff, only: [:tarball, :publish]

  def show
    admin_view = current_user_site_admin? || current_user_staff_for?(@course)
    if admin_view
      submissions = @assignment.used_submissions.includes(:user).order(created_at: :desc).to_a
    elsif @assignment.nil? or @assignment.available > DateTime.now
      redirect_back fallback_location: course_assignments_path, alert: "No such assignment exists or is available"
      return
    else
      submissions = current_user.submissions_for(@assignment).includes(:user).order(created_at: :desc).to_a
    end
    @gradesheet = Gradesheet.new(@assignment, submissions)


    self.send("show_#{@assignment.type.capitalize}")
    if admin_view
      render "show_#{@assignment.type.underscore}"
    else
      @user = current_user
      render "show_user_#{@assignment.type.underscore}"
    end
  end

  def index
  end

  def new
    @assignment = Assignment.new
    @assignment.course_id = @course.id
    @assignment.due_date = (Time.now + 1.week).end_of_day.strftime("%Y/%m/%d %H:%M")
    last_assn = @course.assignments.order(created_at: :desc).first
    if last_assn
      @assignment.points_available = last_assn.points_available
    end
  end

  def edit
  end

  def edit_weights
  end

  def update_weights
    no_problems = true
    params[:weight].each do |kv|
      if !(Float(kv[1]) rescue false)
        @course.errors.add(:base, "#{Assignment.find(kv[0]).name} has non-numeric weight `#{kv[1]}`")
        no_problems = false
      end
    end
    unless no_problems
      render action: "edit_weights"
      return
    end
    params[:weight].each do |kv|
      Assignment.find(kv[0]).update_attribute(:points_available, kv[1])
    end
    redirect_to course_assignments_path
  end

  def create
    @assignment = Assignment.new(assignment_params)
    @assignment.course_id = @course.id
    @assignment.blame_id = current_user.id
    if @assignment.type == "exam"
      @assignment.available = @assignment.due_date
    end

    if set_lateness_config and @assignment.save and set_grader
      @assignment.save_uploads! if params[:assignment][:assignment_file]
      redirect_to course_assignment_path(@course, @assignment), notice: 'Assignment was successfully created.'
    else
      @assignment.destroy
      render action: "new"
    end
  end

  def update
    unless set_lateness_config and set_grader
      render action: "edit"
      return
    end

    ap = assignment_params
    if params[:assignment][:removefile] == "remove"
      ap[:assignment_file] = nil
      @assignment.assignment_upload_id = nil
    end

    if @assignment.update_attributes(ap)
      @assignment.save_uploads! if ap[:assignment_file]
      redirect_to course_assignment_path(@course, @assignment), notice: 'Assignment was successfully updated.'
    else
      render action: "edit"
    end
  end

  def destroy
    @assignment.destroy
    redirect_to @course, notice: "Assignment #{params[:id]} has been deleted."
  end

  def show_user
    assignment = Assignment.find(params[:id])
    if !current_user
      redirect_back fallback_location: root_path, alert: "Must be logged in"
      return
    elsif current_user_site_admin? || current_user_prof_for?(@course)
      # nothing --- but should this be current_user_staff_for instead?
    elsif current_user.id != params[:user_id].to_i
      redirect_back fallback_location: course_assignment_path(@course, assignment),
                    alert: "Not permitted to see submissions for other students"
      return
    end

    @user = User.find(params[:user_id])
    subs = @user.submissions_for(assignment)
    submissions = subs.select("submissions.*").includes(:users).order(created_at: :desc).to_a
    @gradesheet = Gradesheet.new(assignment, submissions)
    render "show_user_#{assignment.type.underscore}"
  end

  def tarball
    tb = SubTarball.new(params[:id])
    if params[:moss]
      tb.update_moss!
    else
      tb.update!
    end
    redirect_to tb.path
  end

  def publish
    used = @assignment.used_submissions
    used.each do |u|
      u.grades.where(score: nil).each do |g| g.grader.grade(@assignment, u) end
      u.grades.update_all(:available => true)
      u.compute_grade!
    end

    redirect_back fallback_location: course_assignment_path(@course, @assignment),
                  notice: 'Grades successfully published'
  end

  def recreate_grades
    count = do_recreate_grades @assignment
    redirect_back fallback_location: course_assignment_path(@course, @assignment),
                  notice: "#{plural(count, 'grade')} created"
  end




  protected

  def set_lateness_config
    lateness = params.to_unsafe_h[:lateness] # FIXME: Should go away in nested-models refactor.
    if lateness.nil?
      @assignment.errors.add(:lateness, "Lateness parameter is missing")
      return false
    end

    type = lateness["type"]
    if type.nil?
      @assignment.errors.add(:lateness, "Lateness type is missing")
      return false
    end
    type = type.split("_")[1]

    lateness = lateness[type]
    #lateness["type"] = type
    if type == "UseCourseDefaultConfig"
      @assignment.lateness_config = @course.lateness_config
    elsif type != "reuse"
      late_config = LatenessConfig.new(lateness.permit(LatenessConfig.attribute_names - ["id"]))
      @assignment.lateness_config = late_config
      late_config.save
    end
    return true
  end

  def set_grader
    return self.send("set_#{params[:assignment][:type]}_graders")
  end


  def do_recreate_grades(assignment)
    confs = assignment.graders.to_a
    count = @assignment.used_submissions.reduce(0) do |sum, sub|
      sum + sub.recreate_missing_grades(confs)
    end
    count
  end

  def assignment_params
    params[:assignment].permit(:name, :assignment, :due_date, :available,
                               :points_available, :hide_grading, :blame_id,
                               :assignment_file,  :type, :related_assignment_id,
                               :course_id, :team_subs, :request_time_taken)
  end

  def graders_params
    graders = params.to_unsafe_h["graders"] # FIXME: Nested models refactor.

    if graders.nil?
      nil
    else
      graders.map do |k, v|
        [k, v]
      end.to_h
    end
  end

  def require_admin_or_prof
    unless current_user_site_admin? || current_user_prof_for?(@course)
      redirect_back fallback_location: course_assignments_path, alert: "Must be an admin or professor."
      return
    end
  end

  def require_admin_or_staff
    unless current_user_site_admin? || current_user_staff_for?(@course)
      redirect_to root_path, alert: "Must be an admin or staff."
      return
    end
  end


  ####################
  # Per-assignment-type actions, by action

  # Show
  def show_Files
  end
  def show_Questions
    @questions = @assignment.questions
  end
  def show_Exam
    @questions = @assignment.questions
    @grader = @assignment.graders.first
  end

  # setup graders
  def set_exam_graders
    upload = params[:assignment][:assignment_file]
    if upload.nil?
      if @assignment.assignment_upload.nil?
        @assignment.errors.add(:base, "Exam questions file is missing")
        return false
      else
        return true
      end
    else
      begin
        questions = YAML.load(upload.tempfile)
        upload.rewind
      rescue Psych::SyntaxError => e
        @assignment.errors.add(:base, "Could not parse the supplied file")
        return false
      end
    end
    if !questions.is_a? Array
      @assignment.errors.add(:base, "Supplied file does not contain a list of questions")
      return false
    else
      @no_problems = true
      @total_weight = 0
      def make_err(msg)
        @assignment.errors.add(:base, msg)
        @no_problems = false
      end
      def is_float(val)
        Float(val) rescue false
      end
      def is_int(val)
        Integer(val) rescue false
      end
      questions.each_with_index do |q, q_num|
        if q["parts"].is_a? Array
          q["parts"].each_with_index do |part, p_num|
            if !is_float(part["weight"])
              make_err "Question #{part['name']} has an invalid weight"
              next
            elsif !part["extra"]
              @total_weight += Float(part["weight"])
            end
          end
        elsif !is_float(q["weight"])
          make_err "Question #{q['name']} has an invalid weight"
          next
        elsif !q["extra"]
          @total_weight += Float(q["weight"])
        end
      end
    end
    if @no_problems
      c = Grader.new(type: "ExamGrader", avail_score: @total_weight)
      if c.invalid? or !c.save
        no_problems = false
        @assignment.errors.add(:graders, "Could not create grader #{c.to_s}")
      else
        AssignmentGrader
          .find_or_initialize_by(assignment_id: @assignment.id, grader_id: c.id)
          .update(order: 1)
      end
    end
    return @no_problems
  end

  def set_questions_graders
    upload = params[:assignment][:assignment_file]
    if upload.nil?
      if @assignment.assignment_upload.nil?
        @assignment.errors.add(:base, "Assignment questions file is missing")
        return false
      else
        return true
      end
    else
      begin
        questions = YAML.load(upload.tempfile)
        upload.rewind
      rescue Psych::SyntaxError => e
        @assignment.errors.add(:base, "Could not parse the supplied file")
        return false
      end
    end
    if !questions.is_a? Array
      @assignment.errors.add(:base, "Supplied file does not contain a list of sections")
      return false
    else
      @question_count = 0
      @total_weight = 0
      question_kinds = Assignment.question_kinds.keys
      @no_problems = true
      def make_err(msg)
        @assignment.errors.add(:base, msg)
        @no_problems = false
      end
      def is_float(val)
        Float(val) rescue false
      end
      def is_int(val)
        Integer(val) rescue false
      end
      questions.each_with_index do |section, index|
        if !(section.is_a? Object) || !(section.keys.is_a? Array) || section.keys.count > 1
          make_err "Section #{index} is malformed"
          next
        else
          section.each do |secName, sec_questions|
            sec_questions.each do |question|
              question.each do |type, q|
                @question_count += 1
                begin
                  if !(type.is_a? String)
                    make_err "Question #{@question_count} (in section #{secName}) has unknown type #{type}"
                    next
                  elsif !question_kinds.member?(type.underscore)
                    make_err "Question #{@question_count} (in section #{secName}) has unknown type #{type}"
                    next
                  else
                    if q["weight"].nil? or !(Float(q["weight"]) rescue false)
                      make_err "Question #{@question_count} has missing or invalid weight"
                    end
                    @total_weight += Float(q["weight"])
                    ans = q["correctAnswer"]
                    if ans.nil?
                      make_err "Question #{@question_count} is missing a correctAnswer"
                    end
                    if q["rubric"].nil?
                      make_err "Question #{@question_count} is missing a rubric"
                    elsif !(q["rubric"].is_a? Array)
                      make_err "Question #{@question_count} has an invalid rubric"
                    else
                      q["rubric"].each_with_index do |guide, i|
                        if !(guide.is_a? Object) or guide.keys.count != 1
                          make_err "Question #{@question_count}, rubric entry #{i} is ill-formed"
                        else
                          guide.each do |weight, hint|
                            if !(Float(weight) rescue false)
                              make_err "Question #{@question_count}, rubric entry #{i} has non-numeric weight"
                            elsif Float(weight) < 0 or Float(weight) > 1
                              make_err "Question #{@question_count}, rubric entry #{i} has out-of-bounds weight"
                            end
                          end
                        end
                      end
                    end
                    if q["prompt"].nil?
                      make_err "Question #{@question_count} is missing a prompt"
                    end
                    case type
                    when "YesNo", "TrueFalse"
                      if ![true, false].member?(q["correctAnswer"])
                        make_err "Boolean question #{@question_count} has a non-boolean correctAnswer"
                      end
                    when "Numeric"
                      min = q["min"]
                      max = q["max"]
                      if max.nil? or !is_float(min)
                        make_err "Numeric question #{@question_count} has a non-numeric max"
                      else
                        max = max.to_f
                      end
                      if min.nil? or !is_float(min)
                        make_err "Numeric question #{@question_count} has a non-numeric min"
                      else
                        min = min.to_f
                      end
                      if ans.nil? or !is_float(ans)
                        make_err "Numeric question #{@question_count} has a non-numeric ans"
                      else
                        ans = ans.to_f
                      end
                      if is_float(min) and is_float(max) and is_float(ans) and !(min <= ans and ans <= max)
                        make_err "Numeric question #{@question_count} has a correctAnswer outside the specified range"
                      end
                    when "MultipleChoice"
                      if q["options"].nil? or !q["options"].is_a? Array
                        make_err "MultipleChoice question #{@question_count} is missing an array of choices"
                      end
                      if !is_int(ans)
                        make_err "MultipleChoice question #{@question_count} has a non-numeric correctAnswer"
                      else
                        ans = ans.to_i
                      end
                      if is_int(ans) and (ans < 0 or ans >= q["options"].count)
                        make_err "MultipleChoice question #{@question_count} has a correctAnswer not in the available choices"
                      end
                    end
                    if q["parts"]
                      if !q["parts"].is_a? Array
                        make_err "Question #{@question_count} has a non-list of parts"
                      else
                        q["parts"].each_with_index do |part, part_i|
                          if !part.is_a? Object
                            make_err "Question #{@question_count} has a non-object part ##{part_i + 1}"
                          elsif part.keys.count > 1
                            make_err "Question #{@question_count} part ##{part_i + 1} has too many keys"
                          elsif !["codeTag", "codeTags", "requiredText", "text"].member?(part.keys[0])
                            make_err "Question #{@question_count} part ##{part_i + 1} has an invalid type #{part.keys[0]}"
                          end
                        end
                      end
                    end
                  end
                rescue Exception => e
                  make_err "Question #{@question_count} in section #{secName} could not be parsed: #{e}"
                end
              end
            end
          end
        end
      end
    end
    if @no_problems
      c = Grader.new(type: "QuestionsGrader", avail_score: @total_weight)
      if c.invalid? or !c.save
        no_problems = false
        @assignment.errors.add(:graders, "Could not create grader #{c.to_s}")
      else
        AssignmentGrader
          .find_or_initialize_by(assignment_id: @assignment.id)
          .update(order: 1, grader_id: c.id)
      end
    end
    return @no_problems
  end

  def set_files_graders
    params_graders = graders_params
    if params_graders.nil?
      @assignment.errors.add(:graders, "parameter is missing")
      return false
    end

    no_problems = true
    graders = {}
    params_graders.each do |id, grader|
      grader[:removed] = true if grader[:removed] == "true"
      grader[:removed] = false if grader[:removed] == "false"
      if grader[:removed]
        graders[id] = grader
        next
      end

      type = grader[:type]
      if type.nil?
        @assignment.errors.add(:graders, "type is missing")
        no_problems = false
        next
      end
      type = type.split("_")[1]

      grader = grader[type]
      grader[:type] = type

      upload = grader[:upload_file]
      if upload
        up = Upload.new
        up.user_id = current_user.id
        up.store_upload!(upload, {
                           type: "#{type} Configuration",
                           date: Time.now.strftime("%Y/%b/%d %H:%M:%S %Z"),
                           mimetype: upload.content_type
                         })
        unless up.save
          @assignment.errors.add(:upload_file, "could not save upload")
          no_problems = false
          next
        end
        grader[:upload_file] = up
      end

      graders[id] = grader
    end
    return no_problems unless no_problems


    Grader.transaction do
      existing_confs = @assignment.graders.to_a
      existing_ags = @assignment.assignment_graders.to_a
      max_order = existing_ags.reduce(0) do |acc, ag| [acc, ag.order].max end
      existing_confs.each do |c|
        conf = graders[c.id.to_s]
        graders.delete c.id.to_s
        if conf[:removed]
          existing_ags.find{|g| g.grader_id == c.id}.destroy
          next
        else
          c.assign_attributes(conf)
          if c.changed?
            unless c.save
              @assignment.errors << c.errors
              no_problems = false
              raise ActiveRecord::Rollback
            end
            # make sure that the graders are updated to be the right total score
            unless c.autograde?
              Grader.where(grader_id: c.id).update_all({out_of: c.avail_score})
            end
          end
        end
      end

      graders.each do |k, conf|
        next if conf[:removed]
        c = Grader.new(conf)
        if c.invalid? or !c.save
          no_problems = false
          @assignment.errors.add(:graders, "Could not create grader #{c.to_s}")
          raise ActiveRecord::Rollback
        else
          AssignmentGrader
            .find_or_initialize_by(assignment_id: @assignment.id, grader_id: c.id)
            .update(order: max_order)
          max_order += 1
        end
      end

      # NOT SURE IF I WANT TO DO THIS IMMEDIATELY OR NOT
      # do_recreate_graders @assignment
    end

    return no_problems
  end


  def find_assignment
    @assignment = Assignment.find_by(id: params[:id])
    if @assignment.nil?
      redirect_back fallback_location: course_assignments_path, alert: "No such assignment"
      return
    end
    if @assignment.course_id != params[:course_id].to_i
      redirect_back fallback_location: course_assignments_path, alert: "No such assignment for this course"
      return
    end
  end
end
