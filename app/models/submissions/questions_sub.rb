require 'securerandom'
require 'audit'

class QuestionsSub < Submission
  include SubmissionsHelper
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
    questions = self.assignment.flattened_questions
    num_qs = questions.count
    if @answers.count != num_qs
      self.errors.add(:base, "There were #{pluralize(@answers.count, 'answer')} for #{pluralize(num_qs, 'question')}")
      self.cleanup!
    else
      check_questions_schema(questions, @answers, questions.count)
    end
  end
end
