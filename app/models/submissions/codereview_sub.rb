require 'securerandom'
require 'audit'

class CodereviewSub < Submission
  include SubmissionsHelper
  has_many :review_feedbacks, foreign_key: "review_submission_id", autosave: true
  validate :check_time_taken
  def check_time_taken
    return unless self.new_record? || self.time_taken_changed?
    if self.assignment.request_time_taken && self.time_taken.blank?
      self.errors.add(:base, "Please specify how long you have worked on this assignment")
    elsif self.time_taken && !(Float(self.time_taken) rescue false)
      self.errors.add(:base, "Please specify a valid number for how long you have worked on this assignment")
    end
    return self.errors.count == 0
  end
  attr_accessor :answers
  def save_upload(prof_override = nil)
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
  def load_answers_related_files(get_line_comments = false)
    return if @answers
    @answers ||= ApplicationHelper.make_yaml_safe(YAML.load(File.read(self.upload.submission_path)))
    @related_files = {}
    @related_subs = self.review_feedbacks.map(&:submission)
    @related_subs.each do |s|
      # we don't need the comments for validation purposes, so by default, don't load them
      _, files = s.get_submission_files(self.user, get_line_comments ? nil : {}) 
      @related_files[s.id] = files
    end
  end
  
  validate :check_all_questions
  def check_all_questions
    return unless self.new_record?
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
    self.related_subs.each do |sub|
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
                                                  upload_id: self.upload.id,
                                                  score: score,
                                                  out_of: out_of
                                                 )
    end
  end

  def dup
    result = super
    self.review_feedbacks.each do |rf|
      rf_dup = rf.dup
      rf_dup.review_submission = result
      result.review_feedbacks << rf_dup
    end
    result
  end
end
