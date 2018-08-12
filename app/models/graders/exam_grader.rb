require 'clamp'
class ExamGrader < Grader
  # Overridden from Grader, because the default is to publish automatically
  def grade(assignment, sub)
    do_grading(assignment, sub)
    # and don't compute_grade automatically; wait to publish
  end

  def autograde!(assignment, sub)
    g = self.grade_for sub

    g.out_of = self.avail_score

    g.updated_at = DateTime.now
    g.available = false
    g.save!

    return nil
  end

  def to_s
    "Exam grading"
  end

  def display_type
    "Exam score"
  end

  def expect_num_questions(num)
    @num_questions = num
  end

  def upload_file=(data)
    unless data.nil?
      errors.add(:base, "You cannot submit a file for exams.")
      return
    end
  end

  protected

  def do_grading(assignment, sub)
    g = self.grade_for sub
    comments = InlineComment.where(submission: sub, grade: g, suppressed: false).where('line < ?', @num_questions)
    score = comments.pluck(:weight).sum

    g.out_of = self.avail_score
    g.score = [0, score].max # can get extra credit above max score

    curved = InlineComment.find_by(submission: sub, grade: g, suppressed: false, line: @num_questions)
    if curved
      g.score = curved.weight
    end

    g.updated_at = DateTime.now
    g.available = false
    g.save!

    if g.score != sub.score
      sub.update(score: nil) # wipe out score until it's republished
    end

    return g.score
  end
end
