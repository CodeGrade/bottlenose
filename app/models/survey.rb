class Survey < Assignment
  enum question_kind: [:yes_no, :true_false, :multiple_choice, :numeric, :text]

  before_create :set_survey_grader

  def sub_not_following_related?(user)
    # Is this submission not coming *after* any submissions to related assignments?
    related = Assignment.where(related_assignment_id: self.id)
    return related.all? do |a|
      a.submissions_for(user).empty?
    end
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

  def set_survey_grader
    # FIXME: This is way too complicated.

    upload = @assignment_file_data
    if upload.nil?
      if self.assignment_upload.nil?
        self.errors.add(:base, "Assignment questions file is missing")
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
      self.errors.add(:base, "Supplied file does not contain a list of sections")
      return false
    else
      question_count = 0
      total_weight = 0
      question_kinds = Assignment.question_kinds.keys
      no_problems = true
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
      questions.each_with_index do |section, index|
        if !(section.is_a? Object) || !(section.keys.is_a? Array) || section.keys.count > 1
          make_err "Section #{index} is malformed"
          next
        else
          section.each do |secName, sec_questions|
            sec_questions.each do |question|
              question.each do |type, q|
                question_count += 1
                begin
                  if !(type.is_a? String)
                    make_err "Question #{question_count} (in section #{secName}) has unknown type #{type}"
                    next
                  elsif !question_kinds.member?(type.underscore)
                    make_err "Question #{question_count} (in section #{secName}) has unknown type #{type}"
                    next
                  else
                    if q["weight"].nil? or !(Float(q["weight"]) rescue false)
                      make_err "Question #{question_count} has missing or invalid weight"
                    end
                    total_weight += Float(q["weight"])
                    ans = q["correctAnswer"]
                    if ans.nil?
                      make_err "Question #{question_count} is missing a correctAnswer"
                    end
                    if q["rubric"].nil?
                      make_err "Question #{question_count} is missing a rubric"
                    elsif !(q["rubric"].is_a? Array)
                      make_err "Question #{question_count} has an invalid rubric"
                    else
                      q["rubric"].each_with_index do |guide, i|
                        if !(guide.is_a? Object) or guide.keys.count != 1
                          make_err "Question #{question_count}, rubric entry #{i} is ill-formed"
                        else
                          guide.each do |weight, hint|
                            if !(Float(weight) rescue false)
                              make_err "Question #{question_count}, rubric entry #{i} has non-numeric weight"
                            elsif Float(weight) < 0 or Float(weight) > 1
                              make_err "Question #{question_count}, rubric entry #{i} has out-of-bounds weight"
                            end
                          end
                        end
                      end
                    end
                    if q["prompt"].nil?
                      make_err "Question #{question_count} is missing a prompt"
                    end
                    case type
                    when "YesNo", "TrueFalse"
                      if ![true, false].member?(q["correctAnswer"])
                        make_err "Boolean question #{question_count} has a non-boolean correctAnswer"
                      end
                    when "Numeric"
                      min = q["min"]
                      max = q["max"]
                      if max.nil? or !is_float(min)
                        make_err "Numeric question #{question_count} has a non-numeric max"
                      else
                        max = max.to_f
                      end
                      if min.nil? or !is_float(min)
                        make_err "Numeric question #{question_count} has a non-numeric min"
                      else
                        min = min.to_f
                      end
                      if ans.nil? or !is_float(ans)
                        make_err "Numeric question #{question_count} has a non-numeric ans"
                      else
                        ans = ans.to_f
                      end
                      if is_float(min) and is_float(max) and is_float(ans) and !(min <= ans and ans <= max)
                        make_err "Numeric question #{question_count} has a correctAnswer outside the specified range"
                      end
                    when "MultipleChoice"
                      if q["options"].nil? or !q["options"].is_a? Array
                        make_err "MultipleChoice question #{question_count} is missing an array of choices"
                      end
                      if !is_int(ans)
                        make_err "MultipleChoice question #{question_count} has a non-numeric correctAnswer"
                      else
                        ans = ans.to_i
                      end
                      if is_int(ans) and (ans < 0 or ans >= q["options"].count)
                        make_err "MultipleChoice question #{question_count} has a correctAnswer not in the available choices"
                      end
                    end
                    if q["parts"]
                      if !q["parts"].is_a? Array
                        make_err "Question #{question_count} has a non-list of parts"
                      else
                        q["parts"].each_with_index do |part, part_i|
                          if !part.is_a? Object
                            make_err "Question #{question_count} has a non-object part ##{part_i + 1}"
                          elsif part.keys.count > 1
                            make_err "Question #{question_count} part ##{part_i + 1} has too many keys"
                          elsif !["codeTag", "codeTags", "requiredText", "text"].member?(part.keys[0])
                            make_err "Question #{question_count} part ##{part_i + 1} has an invalid type #{part.keys[0]}"
                          end
                        end
                      end
                    end
                  end
                rescue Exception => e
                  make_err "Question #{question_count} in section #{secName} could not be parsed: #{e}"
                end
              end
            end
          end
        end
      end
    end
    self.graders ||= []
    self.graders << Grader.new(type: "QuestionsGrader", avail_score: total_weight)
  end
end
