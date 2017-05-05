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
        tap = TapParser.new(File.read(self.grading_output))
      rescue Exception
        return ans
      end
      tap.tests.each do |t|
        by_file = ans[t[:info]["filename"]]
        by_file = ans[t[:info]["filename"]] = {} if by_file.nil?
        by_line = by_file[t[:info]["line"]]
        by_line = by_file[t[:info]["line"]] = {} if by_line.nil?
        by_type = by_line[self.grader_config.type]
        by_type = by_line[self.grader_config.type] = [] if by_type.nil?
        by_type.push t
      end
      ans
    end
  end
end
