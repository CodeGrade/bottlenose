require 'securerandom'
require 'audit'

class ExamSub < Submission
  def grade_question!(who_graded, question, score)
    comment = InlineComment.find_or_initialize_by(submission: self, grade: self.grade, line: question)
    if score.blank?
      comment.delete unless comment.new_record?
      return nil
    else
      comment.update(label: "Exam question",
                     filename: self.assignment.name,
                     severity: InlineComment.severities["info"],
                     user_id: (who_graded || self.user).id,
                     weight: score,
                     comment: "",
                     suppressed: false,
                     title: "",
                     line: question,
                     info: nil)
      comment
    end
  end
  def set_curved_grade(who_graded, curved_grade = nil)
    questions = self.assignment.flattened_questions
    num_questions = questions.count
    if curved_grade.nil? && block_given?
      comments = InlineComment.where(submission: self, suppressed: false).order(:line)
      grades = []
      comments.each do |c|
        if questions[c.line]
          grades[c.line] = {score: c.weight, out_of: questions[c.line]["weight"]}
        else
          grades[c.line] = {curved: c.weight}
        end
      end
      curved_grade = yield(num_questions, grades)
    end
    grader = self.assignment.graders.first
    grade_question!(who_graded, num_questions, curved_grade)
    grader.grade(self.assignment, self)
  end

  protected
  def grade
    return @grade if @grade
    self.assignment.graders.first.ensure_grade_exists_for! self
    @grade = self.grades.first
    return @grade
  end
end
