class InlineComment < ApplicationRecord
  belongs_to :submission
  belongs_to :user
  belongs_to :grade
  enum severity: [:error, :warning, :info, :bonus]
  validates :weight, numericality: true

  def upload_filename
    Upload.upload_path_for(self.filename)
  end

  def to_json
    {
      id: self.id,
      file: self.upload_filename,
      line: self.line,
      author:
        if self.user
          self.user.name
        else
          ""
        end,
      grade: self.grade_id,
      title: self.title,
      label: self.label,
      severity: self.severity.humanize,
      comment: self.comment,
      deduction: self.weight,
      suppressed: self.suppressed,
      info: self.info
    }
  end
  def to_editable_json(comment_author)
    ans = to_json
    ans[:editable] = (self.user && comment_author && (comment_author.id == self.user.id))
    ans
  end
  def to_s(pretty = false, show_file = true)
    if pretty
      ans = ""
      ans += "#{self.filename}, " if show_file
      ans += "#{self.line}:"
      ans += " #{self.user.name} --" if self.user
      ans += " #{self.title}" if self.title.to_s != ""
      ans += self.label if self.user && (self.user.name != self.label)
      ans += " (#{self.severity.humanize}) "
      if self.severity == "bonus"
        w = self.weight
      else
        w = 0 - self.weight
      end
      if self.suppressed
        ans += "[#{w} (ignored)]"
      else
        ans += "[#{w}]"
      end
      ans += "\n\t"
      ans += self.comment
    else
      self.to_json
    end
  end

  def penalty_weight
    # returns how much *to deduct* from the overall score
    # (so negative values mean bonuses)
    if self.severity == "bonus"
      0 - self.weight
    else
      self.weight
    end
  end
end
