class Exam < Assignment
  before_create :set_exam_graders
  before_update :update_exam_submission_times

  def set_exam_graders
    # FIXME: This is complicated, and shouldn't be here either.

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
        questions = YAML.load(upload.tempfile)
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
      no_problems = true
      total_weight = 0
      def make_err(msg)
        self.errors.add(:base, msg)
        no_problems = false
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
              total_weight += Float(part["weight"])
            end
          end
        elsif !is_float(q["weight"])
          make_err "Question #{q['name']} has an invalid weight"
          next
        elsif !q["extra"]
          total_weight += Float(q["weight"])
        end
      end
      return unless no_problems
      c = GraderConfig.new(type: "ExamGrader", avail_score: @total_weight)
      if c.invalid? or !c.save
        self.errors.add(:graders, "Could not create grader #{c.to_s}")
        return false
      else
        AssignmentGrader
          .find_or_initialize_by(assignment_id: self.id, grader_config_id: c.id)
          .update(order: 1)
        return true
      end      
    end
  end

  def update_exam_submission_times
    self.available = self.due_date # for exams, there's no window in which "the assignment is available"
    self.submissions.update_attributes({created_at: self.due_date - 1.minute,
                                        updated_at: self.due_date - 1.minute})
  end

  def questions
    qs = YAML.load(File.read(self.assignment_upload.submission_path))
    qs.each_with_index do |q, q_num|
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
    qs
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
end
