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

  def can_compare_orca_tap?
    return false unless has_orca_output?
    !(self.grading_output.nil? || orca_output['output'].nil?)
  end

  def orca_tap_comparison
    local_tap = TapParser.new(File.read(self.grading_output_path))
    orca_tap = TapParser.new(orca_output['output'])
    unless local_tap.test_count == orca_tap.test_count
      return "Test count does not match; Bottlenose: #{local_tap.test_count} / Orca: #{orca_tap.test_count}"
    end
    local_names_to_weights = local_tap.tests.map { |t| [t[:comment], t[:info]['weight']] }.to_h
    orca_names_to_weights = orca_tap.tests.map { |t| [t[:comment], t[:info]['weight']] }.to_h
    unless local_names_to_weights.keys.sort == orca_names_to_weights.keys.sort
      return 'Bottlenose and Orca TAP test names do not match.'
    end
    unless local_names_to_weights.all? { |n, w| orca_names_to_weights[n] == w }
      return 'Bottlenose and Orca TAP weights do not match.'
    end
    unless local_tap.points_earned == orca_tap.points_earned
      return 'Bottlenose and Orca scores don\'t match. ' \
        "Bottlenose #{local_tap.points_earned} / Orca: #{orca_tap.points_earned}"
    end
    'Bottlenose TAP and Orca TAP match.'
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

  def has_orca_output?
    File.exist? orca_result_path
  end

  def orca_output
    return nil unless has_orca_output?
    JSON.parse(File.open(orca_result_path).read)
  end

  def orca_result_path
    File.join(submission.upload.grader_path(grader), 'result.json')
  end

  def orca_response_url
    url_helpers = Rails.application.routes.url_helpers
    "#{Settings['site_url']}#{url_helpers.orca_response_api_grade_path(self.id)}"
  end

  def submission_grader_dir
    submission.upload.grader_path(grader)
  end
end
