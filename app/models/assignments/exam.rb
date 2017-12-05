class Exam < Assignment
  validates :related_assignment_id, :absence => true
  validate :set_exam_graders
  before_update :update_exam_submission_times

  def assign_attributes(attrs)
    super(attrs)
    self.available = self.due_date
    self.lateness_config = self.course.lateness_config
  end
  
  def set_exam_graders
    return true unless (self.new_record? || self.assignment_upload_id_changed?)
    upload = @assignment_file_data
    if upload.nil?
      if self.assignment_upload.nil?
        self.errors.add(:base, "Exam questions file is missing")
        return false
      else
        return true
      end
    else
      begin
        questions = YAML.load(File.read(upload.tempfile))
        upload.rewind
      rescue Psych::SyntaxError => e
        self.errors.add(:base, "Could not parse the supplied file")
        return false
      end
    end
    if !questions.is_a? Array
      self.errors.add(:base, "Supplied file does not contain a list of questions")
      return false
    else
      @total_weight = 0
      def make_err(msg)
        self.errors.add(:base, msg)
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
      return false if self.errors.count > 0
      grader = self.graders.first
      if grader.nil?
        grader = Grader.new(type: "ExamGrader", assignment: self)
        self.graders << grader
      end
      grader.avail_score = @total_weight
      grader.order = self.graders.count + 1
    end
  end

  def update_exam_submission_times
    self.available = self.due_date # for exams, there's no window in which "the assignment is available"
    self.submissions.update_all({created_at: self.due_date - 1.minute,
                                 updated_at: self.due_date - 1.minute})
  end

  def questions
    return @questions if @questions
    @questions = YAML.load(File.read(self.assignment_upload.submission_path))
    @questions.each_with_index do |q, q_num|
      q["name"] = "Problem #{q_num + 1}" unless q["name"]
      if q["parts"]
        grade = 0
        q["parts"].each_with_index do |p, p_num|
          p["name"] = "Problem #{q_num + 1}#{('a'..'z').to_a[p_num]}" unless p["name"]
          grade += p["weight"]
        end
        q["weight"] = grade unless q["weight"]
      end
    end
    @questions
  end

  def flattened_questions
    qs = self.questions
    flat = []
    qs.each do |q|
      if q["parts"]
        q["parts"].each do |p|
          flat.push p
        end
      else
        flat.push q
      end
    end
    flat
  end

  def sections
    [{name: "", count: self.questions.count}]
  end
end
