require 'securerandom'
require 'audit'

class QuestionsSub < Submission
  validate :check_time_taken
  def check_time_taken
    if self.assignment.request_time_taken && @time_taken.empty?
      self.errors.add(:base, "Please specify how long you have worked on this assignment")
    elsif @time_taken and !(Float(@time_taken) rescue false)
      self.errors.add(:base, "Please specify a valid number for how long you have worked on this assignment")
    end
    return self.errors.count == 0
  end
  attr_accessor :answers
  def save_upload
    if @answers.nil?
      errors.add(:base, "You need to submit a file.")
      return false
    end
    Tempfile.open('answers.yaml', Rails.root.join('tmp')) do |f|
      f.write(YAML.dump(@answers))
      f.flush
      f.rewind
      uploadfile = ActionDispatch::Http::UploadedFile.new(filename: "answers.yaml", tempfile: f)
      self.upload_file = uploadfile
      super
    end
  end
  attr_accessor :related_files
  validate :check_all_questions
  def check_all_questions
    return true unless self.upload.nil? || self.upload.new_record?
    questions = self.assignment.questions.reduce([]) do |acc, section|
      section.reduce(acc) do |acc, (name, qs)| acc + qs end
    end
    num_qs = questions.count
    no_problems = true
    if @answers.count != num_qs
      self.errors.add(:base, "There were #{plural(@answers.count, 'answer')} for #{plural(num_qs, 'question')}")
      self.cleanup!
      no_problems = false
    else
      questions.zip(@answers).each_with_index do |(q, a), i|
        if a.nil? or a["main"].nil?
          self.errors.add(:base, "Question #{i + 1} is missing an answer")
          no_problems = false
          next
        end
        if q["YesNo"]
          type = "YesNo"
          unless ["yes", "no"].member?(a["main"].downcase)
            self.errors.add(:base, "Question #{i + 1} has a non-Yes/No answer")
            no_problems = false
          end
        elsif q["TrueFalse"]
          type = "TrueFalse"
          unless ["true", "false"].member?(a["main"].downcase)
            self.errors.add(:base, "Question #{i + 1} has non-true/false answer")
            no_problems = false
          end
        elsif q["Numeric"]
          type = "Numeric"
          if !(Float(a["main"]) rescue false)
            self.errors.add(:base, "Question #{i + 1} has a non-numeric answer")
            no_problems = false
          elsif Float(a["main"]) < q["Numeric"]["min"] or Float(a["main"]) > q["Numeric"]["max"]
            self.errors.add(:base, "Question #{i + 1} has a numeric answer outside the valid range")
            no_problems = false
          end
        elsif q["MultipleChoice"]
          type = "MultipleChoice"
          if a["main"].nil?
            # nothing, was handled above
          elsif !(Integer(a["main"]) rescue false)
            self.errors.add(:base, "Question #{i + 1} has an invalid multiple-choice answer")
            no_problems = false
          elsif a["main"].to_i < 0 or a["main"].to_i >= q[type]["options"].count
            self.errors.add(:base, "Question #{i + 1} has an invalid multiple-choice answer")
            no_problems = false
          end
        elsif q["Text"]
          type = "Text"
        end
        if q[type]["parts"]
          if a["parts"].nil? or q[type]["parts"].count != a["parts"].count
            self.errors.add(:base, "Question #{i + 1} is missing answers to its sub-parts")
            no_problems = false
          else
            q[type]["parts"].zip(a["parts"]).each_with_index do |(qp, ap), j|
              if qp["codeTag"]
                if self.assignment.related_assignment
                  if ap["file"].to_s == "<none>"
                  # nothing
                  elsif !(@related_files.any?{|f| f[:link] == ap["file"].to_s}) or !(Integer(ap["line"]) rescue false)
                    self.errors.add(:base, "Question #{i + 1} part #{j + 1} has an invalid code-tag")
                    no_problems = false
                  end
                else
                  self.errors.add(:base, "Question #{i + 1} part #{j + 1} has a code-tag, but there is no submission related to these questions!  Please email your professor.")
                  no_problems = false
                end
              elsif qp["codeTags"]
                # TODO
              elsif qp["text"]
                # TODO
              elsif qp["requiredText"]
                if ap["info"].to_s.empty?
                  self.errors.add(:base, "Question #{i + 1} part #{j + 1} has a missing required text answer")
                  no_problems = false
                end
              end
            end
          end
        end
      end
    end
  end
end
