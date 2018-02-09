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

  def guess_who_graded(sub)
    g = grade_for(sub, true)
    return nil if g.new_record?
    return g.inline_comments.first&.user
  end
      

  protected

  def do_grading(assignment, sub)
    g = self.grade_for sub
    comments = InlineComment.where(submission: sub, grade: g, suppressed: false)
    deductions = comments.pluck(:weight).sum

    g.out_of = self.avail_score
    # TODO: Figure out correct clamping behavior.
    g.score = self.avail_score - deductions

    g.updated_at = DateTime.now
    g.available = false
    g.save!

    return g.score
  end

  def recompute_grades
    self.grades.each do |g|
      g.out_of = self.avail_score
      comments = InlineComment.where(submission: g.submission, grade: g, suppressed: false)
      if g.score # only update the computation if there's something already done to update
        g.score = self.avail_score - comments.pluck(:weight).sum
      end
      g.updated_at = DateTime.now
      g.save!
    end
  end
end
