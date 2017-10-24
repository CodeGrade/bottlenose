require 'securerandom'
require 'audit'

class CodereviewSub < Submission
  include SubmissionsHelper
  has_many :review_feedbacks, foreign_key: "review_submission_id", autosave: true
  validate :check_time_taken
  def check_time_taken
    if self.assignment.request_time_taken && self.time_taken.blank?
      self.errors.add(:base, "Please specify how long you have worked on this assignment")
    elsif self.time_taken && !(Float(self.time_taken) rescue false)
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
  attr_accessor :related_subs
  attr_accessor :related_files
  before_validation :load_answers_related_files
  def load_answers_related_files
    return if @answers
    @answers ||= YAML.load(File.read(self.upload.submission_path))
    @related_files = {}
    @related_subs = self.review_feedbacks.map(&:submission)
    @related_subs.each do |s|
      _, files = s.get_submission_files(self.user)
      @related_files[s.id] = files
    end
  end
  
  validate :check_all_questions
  def check_all_questions
    @questions = self.assignment.flattened_questions
    all_questions = @answers.map{|k, _| [k, @questions]}.to_h
    if @answers.count != self.assignment.review_count
      self.errors.add(:base, "There were #{pluralize(@answers.count, 'review')}, but needed #{self.assignment.review_count}")
      self.cleanup!
    else
      bad = @answers.find_all do |k, v|
        v.count != @questions.count
      end
      if bad.count > 0
        bad.each_with_index do |(_, v), i|
          self.errors.add(:base, "Review #{i} needed #{pluralize(@questions.count, 'answer')}, but had #{v.count}")
        end
        self.cleanup!
      else
        check_questions_schema(all_questions, @answers, @questions.count, @related_files)
      end
    end
  end

  before_save :setup_review_feedback
  def setup_review_feedback
    self.review_feedbacks.destroy_all
    load_answers_related_files
    self.related_subs.zip(self.answers.each_slice(self.assignment.flattened_questions.count)).each do |sub, answers|
      Tempfile.open('review.yaml', Rails.root.join('tmp')) do |f|
        f.write(YAML.dump(answers))
        f.flush
        f.rewind
        uploadfile = ActionDispatch::Http::UploadedFile.new(filename: "review.yaml", tempfile: f)
        review_up = Upload.new
        review_up.user_id = self.user_id
        begin
          review_up.store_upload!(uploadfile, {
                                    type: "Codereview",
                                    user: "#{self.user.name} (#{self.user.id})",
                                    course: "#{self.course.name} (#{self.course.id})",
                                    assignment: "#{self.assignment.related_assignment.name} (#{self.assignment.related_assignment.id})",
                                    date: Time.now.strftime("%Y/%b/%d %H:%M:%S %Z"),
                                    mimetype: "text/plain"
                                  })
        rescue Exception => e
          errors.add(:base, e.message)
        end
        review_up.save!

        # Preserve grades if we have them
        grade = self.grades.first
        if grade
          score, out_of = grade.grader.partial_grade_for_sub(self.assignment, grade, sub.id)
        else
          score, out_of = nil, nil
        end
        self.review_feedbacks << ReviewFeedback.new(grade: grade,
                                                    submission_id: sub.id,
                                                    review_submission_id: self.id,
                                                    upload_id: review_up.id,
                                                    score: score,
                                                    out_of: out_of
                                                   )
      end
    end
  end
end
