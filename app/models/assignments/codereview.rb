class Codereview < Assignment
  include AssignmentsHelper
  validate :set_questions_graders
  validates :related_assignment_id, :presence => true
  validate :teamset_consistency

  def prevent_late_submissions
    # Deliberately not using .where(constraint: ...) because this method might be called
    # when some of the interlocks are still new (unsaved) records, which don't show up
    # in a where query
    self.related_interlocks.to_a.keep_if{|i| i.constraint == "no_submission_after_viewing"}
  end
  def prevent_late_submissions?
    !(prevent_late_submissions.empty?)
  end
  def prevent_late_submissions=(aid)
    i = self.related_interlocks.find_or_initialize_by(assignment: self,
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
    return @questions if @questions
    @questions = YAML.load(File.read(self.assignment_upload.submission_path))
    @questions
  end

  def flattened_questions
    qs = self.questions
    flat = []
    qs.each do |section|
      section.each do |name, qs|
        qs.each do |question|
          question.each do |type, q|
            q["type"] = type;
            q["sectionName"] = name
            flat.push q
          end
        end
      end
    end
    flat
  end

  def sections
    self.questions.map do |section|
      {name: section.to_a[0][0], count: section.to_a[0][1].count}
    end
  end

  protected
  def teamset_consistency
    if self.review_target == "self"
      if (self.team_subs && (self.teamset_id != self.related_assignment.teamset_id))
        self.errors.add(:base, "A self-review must have consistent teamsets with the underlying assignment")
        return false
      end
    end
  end
  def set_questions_graders
    return false unless check_questions_schema
    grader = self.graders.first
    if grader.nil?
      grader = Grader.new(type: "CodereviewGrader", assignment: self)
      self.graders << grader
    end
    grader.params = "#{self.review_target};#{self.review_count};#{self.review_threshold}"
    grader.avail_score_will_change! if (@old_weights != @weights)
    grader.avail_score = @total_weight * self.review_count.to_i
    grader.order = self.graders.count + 1
  end
end
