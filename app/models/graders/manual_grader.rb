require 'sub_tarball'
class ManualGrader < Grader
  def autograde!(assignment, sub, prio = 0)
    g = self.grade_for sub

    g.out_of = self.avail_score

    g.updated_at = DateTime.current
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

  def export_data
    used = self.assignment.used_submissions.includes(:users, upload: [:course, :assignment]).map{|s| [s.id, s]}.to_h
    grades = self.grades.where(submission_id: used.keys).includes(inline_comments: [:user])
    tb = SubTarball.new(self.assignment)
    tb.update_with!(
      grades.map do |g|
        upload_path = Upload.upload_path_for(used[g.submission_id].upload.extracted_path)
        comments = g.inline_comments.map do |c|
          {
            file: c.upload_filename.gsub("#{upload_path}/", ""),
            line: c.line,
            author_id: c.user_id,
            author: c.user&.name || "",
            title: c.title,
            label: c.label,
            severity: c.severity,
            comment: c.comment,
            deduction: c.weight,
            suppressed: c.suppressed,
            info: c.info
          }.stringify_keys
        end
        ["#{g.submission_id}.json", JSON.pretty_generate(comments)]
      end.to_h
    )
    tb
  end
  def export_data_schema
    "TBD".html_safe
  end
  def import_data
  end
  def import_data_schema
    "TBD".html_safe
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

    g.updated_at = DateTime.current
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
      g.updated_at = DateTime.current
      g.save!
    end
  end
end
