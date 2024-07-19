require 'tap_parser'

class Grade < ApplicationRecord
  belongs_to :submission
  belongs_to :grader
  has_many :inline_comments

  def complete?
    !self.score.nil?
  end

  def line_comments
    if self.grading_output.nil?
      {}
    else
      ans = {}
      begin
        tap = TapParser.new(File.read(self.grading_output_path))
      rescue Exception
        return ans
      end
      tap.tests.each do |t|
        by_file = ans[t[:info]["filename"]]
        by_file = ans[t[:info]["filename"]] = {} if by_file.nil?
        by_line = by_file[t[:info]["line"]]
        by_line = by_file[t[:info]["line"]] = {} if by_line.nil?
        by_type = by_line[self.grader.type]
        by_type = by_line[self.grader.type] = [] if by_type.nil?
        by_type.push t
      end
      ans
    end
  end

  def full_log
    return nil if self.grading_output.nil?
    go = Pathname.new(self.grading_output)
    self.grading_output&.sub(/#{go.basename}$/, "details.log")
  end

  def grading_output_path
    if self.grading_output
      Upload.full_path_for(self.grading_output)
    else
      nil
    end
  end
  def grading_output_path=(val)
    self.grading_output = Upload.upload_path_for(val)
  end

  def orca_output
    return nil unless File.exist? orca_result_path
    JSON.parse(File.open(orca_result_path).read)
  end

  def orca_result_path
    File.join(submission.upload.grader_path(grader), 'result.json')
  end

  def submission_grader_dir
    submission.upload.grader_path(grader)
  end
end
