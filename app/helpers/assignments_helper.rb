module AssignmentsHelper
  def join(arr, between, before_last=nil)
    ans = ""
    (0..arr.length - 1).each do |i|
      if i > 0
        if i == arr.length - 1 && !before_last.nil?
          ans += before_last
        else
          ans += between
        end
      end
      ans += arr[i].to_s
    end
    ans
  end
  
  def interlock_confirmation(asgn)
    nsavs = asgn.related_interlocks.where(constraint: Interlock::constraints[:no_submission_after_viewing])
    if nsavs.empty?
      {}
    else
      constraints = join(nsavs.map{|i| i.assignment.name}, ", ", " or ")
      { confirm:
          ("Once you view the questions on this assignment, " +
           "you are not allowed to resubmit to #{constraints}. " +
           "Are you sure you want to continue?") }
    end
  end

  def check_questions_schema
    upload = @assignment_file_data
    begin
      case [upload.nil?, self.assignment_upload.nil?]
      when [false, true] # New assignment
        @old_weights = nil
        @questions = YAML.load(upload.read)
        upload.rewind
      when [false, false] # Updated assignment
        old_questions = YAML.load(File.read(self.assignment_upload.submission_path))
        @old_weights = old_questions.map{|section| section.map{|_, qs| qs.map{|q| Float(q.first[1]["weight"])}}}
        @questions = YAML.load(upload.read)
        upload.rewind
      when [true, false] # Existing assignment being re-checked
        @old_weights = nil
        @questions = YAML.load(File.read(self.assignment_upload.submission_path))
      when [true, true] # Impossible
        self.errors.add(:base, "Assignment questions file is missing")
        return false
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
            sec_questions.each_with_index do |question, q_index|
              question.each do |type, q|
                question_count += 1
                question_desc =
                  if question.dig(type, "name")
                    "\"#{question[type]["name"]}\" (question #{q_index + 1} within section #{secName})"
                  else
                    "Question #{question_count} (in section #{secName})"
                  end
                begin
                  if !(type.is_a? String)
                    self.errors.add(:base, "#{question_desc} has unknown type #{type}")
                    next
                  elsif !question_kinds.member?(type.underscore)
                    self.errors.add(:base, "#{question_desc} has unknown type #{type}")
                    next
                  else
                    if q["weight"].nil? || !is_float(q["weight"])
                      self.errors.add(:base, "#{question_desc} has missing or invalid weight")
                    end
                    @total_weight += Float(q["weight"])
                    ans = q["correctAnswer"]
                    if ans.nil?
                      self.errors.add(:base, "#{question_desc} is missing a correctAnswer")
                    end
                    if q["rubric"].nil?
                      self.errors.add(:base, "#{question_desc} is missing a rubric")
                    elsif !(q["rubric"].is_a? Array)
                      self.errors.add(:base, "#{question_desc} has an invalid rubric")
                    else
                      q["rubric"].each_with_index do |guide, i|
                        if !(guide.is_a? Object) || guide.keys.count != 1
                          self.errors.add(:base, "#{question_desc}, rubric entry #{i} is ill-formed: expect an object with exactly one key")
                        else
                          guide.each do |weight, desc|
                            if !is_float(weight)
                              self.errors.add(:base, "#{question_desc}, rubric entry #{i} has non-numeric weight")
                            elsif Float(weight) < 0 || Float(weight) > 1
                              self.errors.add(:base, "#{question_desc}, rubric entry #{i} has out-of-bounds weight")
                            end
                            if desc.is_a? String
                              # ok
                            elsif (desc.is_a? Object) && (desc.keys.sort != ["feedback", "hint"])
                              self.errors.add(:base, "#{question_desc}, rubric entry #{i} has malformed feedback (expected either a String, or a `hint` and a `feedback` message)")
                            end
                          end
                        end
                      end
                    end
                    if q["prompt"].nil?
                      self.errors.add(:base, "#{question_desc} is missing a prompt")
                    end
                    case type
                    when "YesNo", "TrueFalse"
                      if ans && ![true, false].member?(ans)
                        self.errors.add(:base, "Boolean question #{question_desc} has a non-boolean correctAnswer")
                      end
                    when "Numeric"
                      min = q["min"]
                      max = q["max"]
                      if max.nil? || !is_float(min)
                        self.errors.add(:base, "Numeric question #{question_desc} has a non-numeric max")
                      else
                        max = max.to_f
                      end
                      if min.nil? || !is_float(min)
                        self.errors.add(:base, "Numeric question #{question_desc} has a non-numeric min")
                      else
                        min = min.to_f
                      end
                      if ans && !is_float(ans)
                        self.errors.add(:base, "Numeric question #{question_desc} has a non-numeric ans")
                      else
                        ans = ans.to_f
                      end
                      if is_float(min) && is_float(max) && is_float(ans) && !(min <= ans && ans <= max)
                        self.errors.add(:base, "Numeric question #{question_desc} has a correctAnswer outside the specified range")
                      end
                    when "MultipleChoice"
                      if ans && !is_int(ans)
                        self.errors.add(:base, "MultipleChoice question #{question_desc} has a non-numeric correctAnswer")
                      else
                        ans = ans.to_i
                      end
                      if q["options"].nil? || !(q["options"].is_a? Array)
                        self.errors.add(:base, "MultipleChoice question #{question_desc} is missing an array of choices")
                      elsif is_int(ans) && (ans < 0 || ans >= q["options"].count)
                        self.errors.add(:base, "MultipleChoice question #{question_desc} has a correctAnswer not in the available choices")
                      end
                    end
                    if q["parts"]
                      if !q["parts"].is_a? Array
                        self.errors.add(:base, "#{question_desc} has a non-list of parts")
                      else
                        q["parts"].each_with_index do |part, part_i|
                          if !part.is_a? Object
                            self.errors.add(:base, "#{question_desc} has a non-object part ##{part_i + 1}")
                          elsif part.keys.count > 1
                            self.errors.add(:base, "#{question_desc} part ##{part_i + 1} has too many keys")
                          elsif !["codeTag", "codeTags", "requiredText", "text"].member?(part.keys[0])
                            self.errors.add(:base, "#{question_desc} part ##{part_i + 1} has an invalid type #{part.keys[0]}")
                          elsif [Questions].member? self.class && ["codeTag", "codeTags"].member?(part.keys[0])
                            self.errors.add(:base, "#{question_desc} part ##{part_i + 1} asks for #{part.keys[0]}, but there is no related assignment in a Questions assignment")
                          end
                        end
                      end
                    end
                  end
                rescue Exception => e
                  self.errors.add(:base, "#{question_desc} in section #{secName} could not be parsed: #{e}")
                end
              end
            end
          end
        end
      end
      return false if self.errors.count > 0
    end
    @weights = @questions.map{|section| section.map{|_, qs| qs.map{|q| Float(q.first[1]["weight"])}}}
    return true
  end
end
