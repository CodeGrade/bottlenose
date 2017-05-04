require 'clamp'
class ManualGrader < Grader
  def autograde!(assignment, sub)
    g = self.grade_for sub

    g.out_of = self.avail_score

    g.updated_at = DateTime.now
    g.available = false
    g.save!

    return nil
  end

  def display_type
    "Manual Feedback"
  end

  def to_s
    "#{self.avail_score} points: Manual grading"
  end


  protected

  def do_grading(assignment, sub)
    g = self.grade_for sub
    comments = InlineComment.where(submission: sub, grader: g, suppressed: false)
    deductions = comments.pluck(:weight).reduce(0) do |sum, w| sum + w end

    g.out_of = self.avail_score
    # TODO: Figure out correct clamping behavior.
    g.score = self.avail_score - deductions

    g.updated_at = DateTime.now
    g.available = false
    g.save!

    return g.score
  end
end
