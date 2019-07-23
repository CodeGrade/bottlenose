require 'open3'
require 'tap_parser'
require 'audit'
require 'headless'

class PythonStyleGrader < Grader
  def autograde?
    true
  end

  def display_type
    "Python Style"
  end
  
  def to_s
    if self.upload
      "#{self.avail_score} points: Run Python style checker, using #{self.upload.file_name}"
    else
      "#{self.avail_score} points: Run Python style checker"
    end
  end

  after_initialize :load_style_params
  before_validation :set_style_params
  validate :proper_configuration

  def assign_attributes(attrs)
    super
    set_style_params
  end

  def export_data
    tb = SubTarball.new(self.assignment)
    tb.update_with!(
      self.assignment.used_submissions.includes(:users, :grades).where("grades.grader_id": self.id).map do |s|
        g = s.grades.first.grading_output_path
        ["#{s.id}#{File.extname(g)}", g]
      end.to_h
    )
    tb
  end
  def export_data_schema
    <<-schema
An archive containing:
  <submissionId>.tap: the current TAP output for each submission
     OR
  <submissionId>.log: the current log file if there is no TAP output
schema
  end
  def import_data_schema
    <<-schema
An archive containing:
  <submissionId>.tap: the updated TAP output for each submission
                      or the string "DELETE" to erase the grade
schema
  end
  
  protected
  
  def do_grading(assignment, sub)
    g = self.grade_for sub
    Dir.mktmpdir("grade-#{sub.id}-#{g.id}_") do |tmpdir|
      @tmpdir = tmpdir
      sub.upload.extract_contents_to!(nil, Pathname.new(tmpdir))
      Headless.ly(display: g.id % Headless::MAX_DISPLAY_NUMBER, autopick: true) do
        run_command_produce_tap assignment, sub, timeout: Grader::DEFAULT_GRADING_TIMEOUT do |prefix, err, g, tap|
          if err
            record_compile_error(sub, g)
          else
            record_tap_as_comments(g, tap, sub)
          end
        end
      end
    end
  end

  def get_command_arguments(assignment, sub)
    ans = [
      "style.tap",
      {"XDG_RUNTIME_DIR" => nil},
      ["python", Rails.root.join("lib/assets/checkstyle.py").to_s,
       "--grade-config", Rails.root.join("lib/assets/python-config.json").to_s,
       "--max-points", self.avail_score.to_s,
       "--max-line-length", self.line_length.to_s,
       @tmpdir],
      [[@tmpdir, Upload.upload_path_for(sub.upload.extracted_path.to_s)]].to_h
    ]
    if self.upload&.submission_path && File.file?(self.upload.submission_path)
      ans[2].insert(4,
                    "--grade-config", self.upload.submission_path.to_s)
    end
    ans
  end

  def load_style_params
    return if new_record?
    self.line_length = self.params.to_i
  end

  def set_style_params
    self.params = "#{self.line_length}"
  end

  def recompute_grades
    # nothing to do:
    # we already compute the score here based on the TAP output
  end

  def proper_configuration
    return if self.upload.nil?
    begin
      contents = self.upload.upload_data
      json = JSON.parse(contents)
      if !json.is_a? Hash
        add_error("configuration file is not a valid dictionary")
      else
        json.each do |k, v|
          case k
          when "maximum deductions per file"
            if !is_int(v)
              add_error("maximum deductions per file is not an integer")
            end
          when "initial points"
            if !is_float(v)
              add_error("maximum deductions per file is not an integer")
            end
          when /E\d*/, /W\d*/
            if !v.is_a? Hash
              add_error("configuration for #{k} is not a valid dictionary")
            else
              v.each do |prop, val|
                case prop
                when "severity"
                  if !["error", "warning", "info", "ignore"].member?(val.downcase)
                    add_error("configuration for #{k} has unknown severity #{val}")
                  end
                when "description"
                  if !val.is_a? String
                    add_error("configuration for #{k} has a non-string description #{val}")
                  end
                when "maximumDeductions"
                  if !(Integer(val) rescue false)
                    add_error("configuration for #{k} has non-integer maximum number of deductions #{val}")
                  end
                when "deduction"
                  if !(Float(val) rescue false)
                    add_error("configuration for #{k} has non-numeric deduction #{val}")
                  end
                else
                  add_error("configuration for #{k} has unknown property #{prop}")
                end
              end
            end
          else
            add_error("configuration file has an unknown key: #{k}")
          end
        end
      end
    rescue Exception => e
      e_msg = e.to_s
      e_msg = e_msg.dump[1...-1] unless e_msg.is_utf8?
      add_error("configuration file is invalid JSON: #{e_msg}")
    end
    return self.errors.empty?
  end
end
