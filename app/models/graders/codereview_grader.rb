require 'clamp'
class CodereviewGrader < Grader
  def review_target
    @review_target
  end
  def review_target=(value)
    params_will_change! if review_target != value
    @review_target = value
  end
  def review_count
    @review_count
  end
  def review_count=(value)
    params_will_change! if review_count != value
    @review_count = value
  end
  def review_threshold
    @review_threshold
  end
  def review_threshold=(value)
    params_will_change! if review_threshold != value
    @review_threshold = value
  end

  after_initialize :load_review_params
  before_validation :set_review_params

  def assign_attributes(attrs)
    super
    set_review_params
  end
  
  
  def autograde!(assignment, sub)
    g = self.grade_for sub

    g.out_of = self.avail_score

    g.updated_at = DateTime.now
    g.available = false
    g.save!

    return nil
  end

  def display_type
    "Codereview Grading"
  end

  def to_s
    "Codereview grading"
  end

  def partial_grade_for_sub(assignment, g, sub_id)
    if g&.grading_output
      @responses ||= YAML.load(File.read(g.grading_output))
      questions = assignment.flattened_questions
      score = @responses[sub_id.to_s].zip(questions).reduce(0) do |sum, (a, q)|
        sum + (q["weight"].to_f.clamp(0, 1) * a["score"].to_f)
      end
      total = questions.reduce(0){|sum, q| sum + q["weight"].to_f}
      return score, total
    else
      return nil, nil
    end
  end

  def guess_who_graded(subs)
    grades = Grade.where(grader_id: self, submission: subs).where.not(grading_output: nil).group_by(&:submission_id)
    return grades.map do |sub_id, g|
      [sub_id, YAML.load(File.open(g.first.grading_output))["grader"].to_i]
    end.to_h
  end
  
  protected

  def do_grading(assignment, sub)
    g = self.grade_for sub
    responses = YAML.load(File.read(g.grading_output))
    responses.delete("grader")
    questions = assignment.flattened_questions
    score = responses.values.flatten.zip(questions * assignment.review_count).reduce(0) do |sum, (r, q)|
      sum + (r["score"].to_f.clamp(0, 1) * q["weight"])
    end

    g.out_of = self.avail_score
    g.score = score.clamp(0, self.avail_score)

    g.updated_at = DateTime.now
    g.available = false
    g.save!

    return g.score
  end

  def load_review_params
    return if new_record?
    reviewTarget, reviewCount, reviewThreshold = self.params.to_s.split(";")
    self.review_target = reviewTarget
    self.review_count = reviewCount&.to_i
    self.review_threshold = reviewThreshold&.to_i
  end

  def set_review_params
    self.params = "#{self.review_target};#{self.review_count};#{self.review_threshold}"
  end

  def recompute_grades
    self.grades.each do |g|
      self.do_grading(self.assignment, g.submission) if g.score
    end
  end
end
