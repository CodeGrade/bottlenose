class Exam < Assignment
  before_create :set_exam_grader

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

  def set_exam_grader
    # FIXME: This is still too complicated for a model.

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
    end
    self.graders ||= []
    self.graders << Grader.new(type: "ExamGrader", avail_score: total_weight)
  end
end
