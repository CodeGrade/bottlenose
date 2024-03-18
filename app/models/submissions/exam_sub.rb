require 'securerandom'
require 'audit'

class ExamSub < Submission
  def grade_question!(who_graded, question, score)
    comment = find_or_initialize_cached_comment_by(self.grade, question)
    if score.blank?
      comment.delete unless comment.new_record?
      @comments_map[grade.id][question] = nil if @comments_map&.dig(grade.id)
      inline_comments.delete(comment)
      return nil
    else
      comment.update(grading_comment_attributes(who_graded, self.grade, question, score))
      comment
    end
  end
  def grading_comment_attributes(who_graded, grade, question, score)
    return nil if score.blank?
    {
      id: @comments_map&.dig(grade.id, question)&.id,
      submission_id: self.id,
      grade_id: grade.id,
      label: "Exam question",
      filename: self.assignment.name,
      severity: InlineComment.severities["info"],
      user_id: (who_graded || self.user).id,
      weight: score,
      comment: "",
      suppressed: false,
      title: "",
      line: question
    }.compact
  end
  def set_curved_grade(who_graded, curved_grade = nil)
    questions = self.assignment.flattened_questions
    num_questions = questions.count
    if curved_grade.nil? && block_given?
      comments = inline_comments.reject{|c| c.suppressed}.sort_by(&:line)
      grades = []
      curved = nil
      comments.each do |c|
        if questions[c.line]
          grades[c.line] = {score: c.weight, out_of: questions[c.line]["weight"]}
        else
          curved = c.weight
        end
      end
      curved_grade = yield(num_questions, grades, curved)
      return if curved_grade == false # Explicitly don't change the curved score
    end
    grader = self.assignment.graders.first
    grade_question!(who_graded, num_questions, curved_grade)
    grader.expect_num_questions(num_questions)
    grader.grade(self.assignment, self)
  end

  def cache_grading_comments!
    @comments_map = multi_group_by(inline_comments, [:grade_id, :line], true)
  end

  def find_or_initialize_cached_comment_by(grade, question)
    ans = @comments_map&.dig(grade.id, question)
    if ans.nil?
      @comments_map ||= {}
      @comments_map[grade.id] ||= {}
      ans = @comments_map[grade.id][question] = inline_comments.find_or_create_by(grade: grade, line: question)
    end
    ans
  end

  protected
  def grade
    return @grade if @grade
    @grade = self.grades.first
    return @grade if @grade
    self.assignment.graders.first.ensure_grade_exists_for! self
    @grade = self.grades.first
    return @grade
  end
end
