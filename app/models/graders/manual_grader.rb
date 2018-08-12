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

  def guess_who_graded(subs)
    grades = Grade.where(grader_id: self.id, submission: subs).where.not(score: nil)
    comments = InlineComment.where(grade_id: grades.map(&:id))
               .order(:submission_id)
               .select("submission_id",
                       "min(inline_comments.user_id) as user_id").group("submission_id")
    return comments.map{|c| [c.submission_id, c.user_id]}.to_h
  end
      

  protected

  def do_grading(assignment, sub)
    g = self.grade_for sub
    comments = InlineComment.where(submission: sub, grade: g, suppressed: false)
    deductions = comments.sum(&:penalty_weight)

    g.out_of = self.avail_score
    # TODO: Figure out correct clamping behavior.
    if self.extra_credit?
      g.score = 0 - deductions
    else
      g.score = self.avail_score - deductions
    end

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
        deductions = comments.sum(&:penalty_weight)
        if self.extra_credit?
          g.score = 0 - deductions
        else
          g.score = self.avail_score - deductions
        end
      end
      g.updated_at = DateTime.now
      g.save!
    end
  end
end
