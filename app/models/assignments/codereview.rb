class Codereview < Assignment
  validate :set_questions_graders
  validates :related_assignment_id, :presence => true
  validate :teamset_consistency

  def prevent_late_submissions
    !self.interlocks.where(constraint: Interlock::constraints[:no_submission_after_viewing]).empty?
  end
  def prevent_late_submissions=(aid)
    i = self.interlocks.find_or_initialize_by(related_assignment: self,
                                              constraint: Interlock::constraints[:no_submission_after_viewing])
    i.assignment_id = aid
  end

  def review_count
    self.graders.first.review_count.to_i
  end
  def review_target
    self.graders.first.review_target
  end
  def review_threshold
    self.graders.first.review_threshold.to_i
  end
  
  def questions
    YAML.load(File.read(self.assignment_upload.submission_path))
  end

  def flattened_questions
    qs = self.questions
    flat = []
    qs.each do |section|
      section.each do |name, qs|
        qs.each do |question|
          question.each do |type, q|
            q["type"] = type;
            flat.push q
          end
        end
      end
    end
    flat
  end

  protected
  def setup_interlocks
    debugger
  end
  def teamset_consistency
    if self.review_target == "self"
      if (self.team_subs && (self.teamset_id != self.related_assignment.teamset_id)) ||
         (!self.team_subs && self.related_assignment.team_subs)
        self.errors.add(:base, "A self-review must have consistent teamsets with the underlying assignment")
        return false
      end
    end
  end
  def set_questions_graders
    upload = @assignment_file_data
    if upload.nil?
      if self.assignment_upload.nil?
        self.errors.add(:base, "Assignment questions file is missing")
        return false
      else
        # continue
      end
    end
    begin
      if !upload.nil?
        @questions = YAML.load(File.read(upload.tempfile))
        upload.rewind
      else
        @questions = YAML.load(File.read(self.assignment_upload.submission_path))
      end
    rescue Psych::SyntaxError => e
      self.errors.add(:base, "Could not parse the supplied file: #{e}")
      return false
    end
    if !@questions.is_a? Array
      self.errors.add(:base, "Supplied file does not contain a list of sections")
      return false
    else
      question_count = 0
      @total_weight = 0
      question_kinds = Assignment.question_kinds.keys
      def is_float(val)
        Float(val) rescue false
      end
      def is_int(val)
        Integer(val) rescue false
      end
      @questions.each_with_index do |section, index|
        if !(section.is_a? Object) || !(section.keys.is_a? Array) || section.keys.count > 1
          self.errors.add(:base, "Section #{index} is malformed")
          next
        else
          section.each do |secName, sec_questions|
            sec_questions.each do |question|
              question.each do |type, q|
                question_count += 1
                begin
                  if !(type.is_a? String)
                    self.errors.add(:base, "Question #{question_count} (in section #{secName}) has unknown type #{type}")
                    next
                  elsif !question_kinds.member?(type.underscore)
                    self.errors.add(:base, "Question #{question_count} (in section #{secName}) has unknown type #{type}")
                    next
                  else
                    if q["weight"].nil? || !is_float(q["weight"])
                      self.errors.add(:base, "Question #{question_count} has missing or invalid weight")
                    end
                    @total_weight += Float(q["weight"])
                    ans = q["correctAnswer"]
                    if ans.nil?
                      self.errors.add(:base, "Question #{question_count} is missing a correctAnswer")
                    end
                    if q["rubric"].nil?
                      self.errors.add(:base, "Question #{question_count} is missing a rubric")
                    elsif !(q["rubric"].is_a? Array)
                      self.errors.add(:base, "Question #{question_count} has an invalid rubric")
                    else
                      q["rubric"].each_with_index do |guide, i|
                        if !(guide.is_a? Object) || guide.keys.count != 1
                          self.errors.add(:base, "Question #{question_count}, rubric entry #{i} is ill-formed")
                        else
                          guide.each do |weight, desc|
                            if !is_float(weight)
                              self.errors.add(:base, "Question #{question_count}, rubric entry #{i} has non-numeric weight")
                            elsif Float(weight) < 0 || Float(weight) > 1
                              self.errors.add(:base, "Question #{question_count}, rubric entry #{i} has out-of-bounds weight")
                            end
                            if desc.is_a? String
                              # ok
                            elsif (desc.is_a? Object) && !((desc["hint"].is_a? String) || (desc["feedback"].is_a? String))
                              self.errors.add(:base, "Question #{question_count}, rubric entry #{i} has malformed feedback")
                            end
                          end
                        end
                      end
                    end
                    if q["prompt"].nil?
                      self.errors.add(:base, "Question #{question_count} is missing a prompt")
                    end
                    case type
                    when "YesNo", "TrueFalse"
                      if ![true, false].member?(q["correctAnswer"])
                        self.errors.add(:base, "Boolean question #{question_count} has a non-boolean correctAnswer")
                      end
                    when "Numeric"
                      min = q["min"]
                      max = q["max"]
                      if max.nil? || !is_float(min)
                        self.errors.add(:base, "Numeric question #{question_count} has a non-numeric max")
                      else
                        max = max.to_f
                      end
                      if min.nil? || !is_float(min)
                        self.errors.add(:base, "Numeric question #{question_count} has a non-numeric min")
                      else
                        min = min.to_f
                      end
                      if ans.nil? || !is_float(ans)
                        self.errors.add(:base, "Numeric question #{question_count} has a non-numeric ans")
                      else
                        ans = ans.to_f
                      end
                      if is_float(min) && is_float(max) && is_float(ans) && !(min <= ans and ans <= max)
                        self.errors.add(:base, "Numeric question #{question_count} has a correctAnswer outside the specified range")
                      end
                    when "MultipleChoice"
                      if q["options"].nil? || !(q["options"].is_a? Array)
                        self.errors.add(:base, "MultipleChoice question #{question_count} is missing an array of choices")
                      end
                      if !is_int(ans)
                        self.errors.add(:base, "MultipleChoice question #{question_count} has a non-numeric correctAnswer")
                      else
                        ans = ans.to_i
                      end
                      if is_int(ans) && (ans < 0 || ans >= q["options"].count)
                        self.errors.add(:base, "MultipleChoice question #{question_count} has a correctAnswer not in the available choices")
                      end
                    end
                    if q["parts"]
                      if !q["parts"].is_a? Array
                        self.errors.add(:base, "Question #{question_count} has a non-list of parts")
                      else
                        q["parts"].each_with_index do |part, part_i|
                          if !part.is_a? Object
                            self.errors.add(:base, "Question #{question_count} has a non-object part ##{part_i + 1}")
                          elsif part.keys.count > 1
                            self.errors.add(:base, "Question #{question_count} part ##{part_i + 1} has too many keys")
                          elsif !["codeTag", "codeTags", "requiredText", "text"].member?(part.keys[0])
                            self.errors.add(:base, "Question #{question_count} part ##{part_i + 1} has an invalid type #{part.keys[0]}")
                          end
                        end
                      end
                    end
                  end
                rescue Exception => e
                  self.errors.add(:base, "Question #{question_count} in section #{secName} could not be parsed: #{e}")
                end
              end
            end
          end
        end
      end
      return false if self.errors.count > 0
      if self.graders.empty?
        self.graders << Grader.new(type: "CodereviewGrader", assignment: self)
      end
      self.graders[0].params = "#{self.review_target};#{self.review_count};#{self.review_threshold}"
      self.graders[0].avail_score = @total_weight * self.review_count.to_i
      self.graders[0].order = self.graders.count + 1
    end
  end
end
