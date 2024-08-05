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
    !(self.bottlenose_tap.nil? || orca_output['output'].nil?)
  end

  def orca_tap_matches_bottlenose?
    return false unless tap_test_counts_match? && tap_test_scores_match?

    bottlenose_hash, orca_hash = tap_tests_hash(bottlenose_tap), tap_tests_hash(orca_tap)
    return false unless (bottlenose_hash.keys - orca_hash.keys).empty?

    bottlenose_hash.keys.all? { |k| orca_hash[k] == bottlenose_hash[k] }
  end

  def orca_tap_comparison
    unless tap_test_counts_match?
      return "Test count does not match; Bottlenose: #{local_tap.test_count} | Orca: #{orca_tap.test_count}"
    end

    unless tap_test_scores_match?
      return 'Bottlenose and Orca scores don\'t match. ' \
        "Bottlenose #{bottlenose_tap.points_earned} / #{bottlenose_tap.points_available} " \
        "| Orca: #{orca_tap.points_earned} / #{orca_tap.points_available}"
    end

    bottlenose_hash, orca_hash = tap_tests_hash(bottlenose_tap), tap_tests_hash(orca_tap)

    unless (bottlenose_hash.keys - orca_hash.keys).empty?
      return 'Bottlenose and Orca TAP test names do not match.'
    end

    unless bottlenose_hash.all? { |n, w| orca_hash[n] == w }
      return 'Bottlenose and Orca TAP weights do not match.'
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

  protected

  def tap_tests_hash(tap)
    h = {}
    tap.tests.each do |t|
      (h[t[:comment]] ||= []) << t[:info]['weight']
    end
    h
  end

  def tap_test_scores_match?
    orca_tap.points_earned == bottlenose_tap.points_earned &&
      orca_tap.points_available == bottlenose_tap.points_available
  end

  def tap_test_counts_match?
    orca_tap.test_count == bottlenose_tap.test_count
  end

  def orca_tap
    return nil if orca_output.nil? || orca_output['output'].nil?

    TapParser.new(orca_output['output'])
  end

  def bottlenose_tap
    return nil if self.grading_output.nil?

    TapParser.new(File.read(self.grading_output_path))
  end
end
