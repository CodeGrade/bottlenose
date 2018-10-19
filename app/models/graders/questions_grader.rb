require 'clamp'
class QuestionsGrader < Grader
  def autograde!(assignment, sub, prio = 0)
    g = self.grade_for sub

    g.out_of = self.avail_score

    g.updated_at = DateTime.now
    g.available = false
    g.save!

    return nil
  end

  def display_type
    "Question Grading"
  end

  def to_s
    "Question grading"
  end


  def guess_who_graded(subs)
    grades = Grade.where(grader_id: self, submission: subs).where.not(grading_output: nil).group_by(&:submission_id)
    return grades.map do |sub_id, g|
      [sub_id, YAML.load(File.open(g.first.grading_output_path))["grader"].to_i]
    end.to_h
  end

  protected

  def do_grading(assignment, sub)
    g = self.grade_for sub
    comments = InlineComment.where(submission: sub, grade: g, suppressed: false).order(:line)
    questions = assignment.flattened_questions
    score = comments.pluck(:weight).zip(questions).reduce(0) do |sum, (w, q)|
      sum + (w.clamp(0, 1) * q["weight"])
    end

    g.out_of = self.avail_score
    g.score = score.clamp(0, self.avail_score)

    g.updated_at = DateTime.now
    g.available = false
    g.save!

    return g.score
  end

  def recompute_grades
    self.grades.each do |g|
      self.do_grading(self.assignment, g.submission) if g.score
    end
  end
end
