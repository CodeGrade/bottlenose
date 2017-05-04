require 'clamp'
class QuestionsGrader < Grader
  def autograde!(assignment, sub)
    g = self.grader_for sub

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


  protected

  def do_grading(assignment, sub)
    g = self.grader_for sub
    comments = InlineComment.where(submission: sub, grader: g, suppressed: false).order(:line)
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
end
