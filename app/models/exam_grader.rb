require 'clamp'
class ExamGrader < Grader
  def autograde!(assignment, sub)
    g = self.grader_for sub
    
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
      errors[:base] << "You cannot submit a file for exams."
      return
    end
  end
  
  protected
  
  def do_grading(assignment, sub)
    g = self.grader_for sub
    comments = InlineComment.where(submission: sub, grader: g, suppressed: false).where('line < ?', @num_questions)
    score = comments.pluck(:weight).sum
    
    g.out_of = self.avail_score
    g.score = [0, score].max # can get extra credit above max score

    curved = InlineComment.find_by(submission: sub, grader: g, suppressed: false, line: @num_questions)
    if curved
      g.score = curved.weight
    end
    
    g.updated_at = DateTime.now
    g.available = ((comments.count == @num_questions) or !curved.nil?)
    g.save!

    return g.score
  end
end
