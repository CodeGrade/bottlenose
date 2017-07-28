module SubmissionsHelper
  def check_questions_schema(questions, answers, questions_count)
    questions.keys.each_with_index do |sub_id, sub_num|
      questions[sub_id].zip(answers[sub_id]).each_with_index do |(q, a), i|
        prefix = "Section #{sub_num + 1}, question #{i + 1}"
        if a.nil? || a["main"].nil?
          self.errors.add(:base, "#{prefix} is missing an answer")
          next
        end
        if q["YesNo"]
          unless ["yes", "no"].member?(a["main"].downcase)
            self.errors.add(:base, "#{prefix} has a non-Yes/No answer")
          end
        elsif q["TrueFalse"]
          unless ["true", "false"].member?(a["main"].downcase)
            self.errors.add(:base, "#{prefix} has non-true/false answer")
          end
        elsif q["Numeric"]
          if !(Float(a["main"]) rescue false)
            self.errors.add(:base, "#{prefix} has a non-numeric answer")
          elsif Float(a["main"]) < q["Numeric"]["min"] || Float(a["main"]) > q["Numeric"]["max"]
            self.errors.add(:base, "#{prefix} has a numeric answer outside the valid range")
          end
        elsif q["MultipleChoice"]
          if a["main"].nil?
          # nothing, was handled above
          elsif !(Integer(a["main"]) rescue false)
            self.errors.add(:base, "#{prefix} has an invalid multiple-choice answer")
          elsif a["main"].to_i < 0 || a["main"].to_i >= q[type]["options"].count
            self.errors.add(:base, "#{prefix} has an invalid multiple-choice answer")
          end
        elsif q["Text"]
          # nothing
        end
        if q["parts"]
          if a["parts"].nil? || q["parts"].count != a["parts"].count
            self.errors.add(:base, "#{prefix} is missing answers to its sub-parts")
          else
            q["parts"].zip(a["parts"]).each_with_index do |(qp, ap), j|
              if qp["codeTag"]
                if self.assignment.related_assignment
                  if ap["file"].to_s == "<none>"
                  # nothing
                  else
                    file = @related_files[sub_num].find{|f| f[:link] == ap["file"].to_s}
                    line_num = (Integer(ap["line"]) rescue nil)
                    puts file[:link]
                    puts line_num
                    puts file[:contents].lines.count
                    if file.nil? || line_num.nil?
                      self.errors.add(:base, "#{prefix} part #{j + 1} has an invalid code-tag")
                    elsif (line_num < 1 || line_num > file[:contents].lines.count)
                      self.errors.add(:base, "#{prefix} part #{j + 1} has an invalid line number")
                    end
                  end
                else
                  self.errors.add(:base, "#{prefix} part #{j + 1} has a code-tag, but there is no submission related to these questions!  Please email your professor.")
                end
              elsif qp["codeTags"]
              # TODO
              elsif qp["text"]
              # TODO
              elsif qp["requiredText"]
                if ap["info"].to_s.empty?
                  self.errors.add(:base, "#{prefix} part #{j + 1} has a missing required text answer")
                end
              end
            end
          end
        end
      end
    end
  end
end
