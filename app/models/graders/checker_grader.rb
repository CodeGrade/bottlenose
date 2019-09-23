require 'open3'
require 'tap_parser'
require 'audit'
require 'headless'

class CheckerGrader < Grader
  after_initialize :load_checker_params
  before_validation :set_checker_params
  validate :proper_configuration

  def autograde?
    true
  end

  def display_type
    "Checker Tests"
  end
  
  def to_s
    if self.upload
      filename = self.upload.file_name
    else
      filename = "<no file>"
    end
    "#{self.avail_score} points: Run Checker tests in #{test_class} from #{filename} " +
      "(with default timeout #{test_timeout}), " +
      "and show #{pluralize(errors_to_show, 'failed test')}"
  end

  def export_data
    export_tap_data
  end
  def export_data_schema
    "checker_export_schema"
  end
  def import_data(who_grades, file)
    import_tap_data(who_grades, file) do |g, raw_tap, sub|
      if File.extname(g.grading_output_path) == ".tap"
        tap = TapParser.new(raw_tap)
        g.score = tap.points_earned
        g.out_of = tap.points_available
      else
        g.score = 0
        g.out_of = self.avail_score
      end
      g.updated_at = DateTime.now
      g.available = true
    end
  end
  def import_data_schema
    "checker_import_schema"
  end

  protected
  def load_checker_params
    return if new_record?
    testClass, errorsToShow, testTimeout = self.params.to_s.split(";")
    self.test_class = testClass
    self.errors_to_show = errorsToShow.to_i
    self.test_timeout = (testTimeout || Grader::DEFAULT_TEST_TIMEOUT).to_i
  end
  def set_checker_params
    self.params = "#{self.test_class};#{self.errors_to_show};#{self.test_timeout}"
  end
    
  def do_grading(assignment, sub)
    g = self.grade_for sub
    Dir.mktmpdir("grade-#{sub.id}-#{g.id}_") do |build_dir|
      @build_dir = build_dir
      begin
        Headless.ly(display: g.id % Headless::MAX_DISPLAY_NUMBER, autopick: true) do
          run_build_produce_problems assignment, sub do |prefix, any_problems, details|
            if any_problems
              g.grading_output_path = details.path
              g.score = 0
              g.out_of = self.avail_score
              g.updated_at = DateTime.now
              g.available = true
              g.save!
            else
              timeout = Grader::DEFAULT_GRADING_TIMEOUT
              # If the professor supplied a per-test timeout, then we can relax our overall timeout a bit
              if self.test_timeout then timeout = timeout * 5 end
              run_command_produce_tap assignment, sub, timeout: timeout do |prefix, err, g, tap|
                if tap
                  g.score = tap.points_earned
                  g.out_of = tap.points_available
                  g.updated_at = DateTime.now
                  g.available = true
                  g.save!
                  Audit.log("#{prefix}: Tests give raw score of #{g.score} / #{g.out_of}")
                else
                  if err[:output]
                    details.write("Test output: (exit status #{err[:status] || 'unknown'})\n")
                    details.write(err[:output])
                  end
                  if err[:timed_out]
                    details.write("Running tests timed out after #{self.test_timeout} seconds")
                  end
                  Audit.log("#{prefix}: Errors prevented grading; giving a 0")
                  g.grading_output_path = details.path
                  g.score = 0
                  g.out_of = self.avail_score
                  g.updated_at = DateTime.now
                  g.available = true
                  g.save!
                end
              end
            end
          end
        end
      rescue Exception => e
        Audit.log("Assignment #{assignment.id}, submission #{sub.id}: Errors prevented grading; giving a 0: #{e}")
        g.score = 0
        g.out_of = self.avail_score
        g.updated_at = DateTime.now
        g.available = true
        g.save!
      end
    end
  end

  def get_extraction_arguments(assignment, sub)
    [
      @build_dir,
      self.upload.extracted_path,
      sub.upload.extracted_path,
      [
        Rails.root.join("lib/assets/tester-3.0.jar"),
        Rails.root.join("lib/assets/javalib.jar")
      ]
    ]
  end

  def get_build_arguments(assignment, sub)
    files = Dir.glob("#{@build_dir}/**/*.java").reject do |f|
      Pathname.new(f).ascend.any?{|c| c.basename.to_s == "__MACOSX" || c.basename.to_s == ".DS_STORE"}
    end
    [
      "details.log",
      {},
      files.map do |f| ["javac", "-cp", "tester-3.0.jar:javalib.jar:.:./*", f, *files] end,
      {},
      @build_dir
    ]
  end

  def get_command_arguments(assignment, sub)
    [
      "checker.tap",
      {},
      ["java", "-XX:MaxJavaStackTraceDepth=1000000",
       "-cp", "tester-3.0.jar:javalib.jar:.:./*", "tester.Main",
       "-secmon", "-tap", "-enforceTimeouts",
       "-defaultTimeout", self.test_timeout.to_s,
       *(self.test_class.split(" "))],
      {},
      @build_dir
    ]
  end

  def recompute_grades
    # nothing to do:
    # we already compute the score here based on the TAP output
  end

  def proper_configuration
    if self.upload.nil?
      add_error("Upload cannot be nil")
    else
      if self.test_class.blank?
        add_error("Test class cannot be blank")
      end
      if self.errors_to_show.blank?
        add_error("Errors to show cannot be blank")
      end
      if self.test_timeout.blank?
        add_error("Default test timeout cannot be blank")
      end
    end
    return if self.upload.nil? || self.test_class.blank?
    begin
      entries = self.upload.upload_entries
      if entries.size == 1 && entries.keys.any?{|k| k.ends_with? ".java"}
        if !self.upload.upload_data.match? /class #{self.test_class}\b/
          add_error("The uploaded Java file does not contain the specified test class #{self.test_class}")
        end
      elsif entries["starter"] && entries["testing"]
        ok = true
        if entries["starter"].size > 0 && !(entries["starter"]["src"] && entries["starter"]["test"])
          add_error("The starter/ directory does not contain src/ and test/ subdirectories")
          ok = false
        end
        if !(entries["testing"]["src"] && entries["testing"]["test"])
          add_error("The testing/ directory does not contain src/ and test/ subdirectories")
          ok = false
        end
        if ok
          self.test_class.split.each do |tc|
            next if (tc.starts_with? "-" || (Float(tc) rescue false))
            if !entries["testing"]["test"]["#{tc}.java"]
              add_error("There is no #{tc}.java file to match the specified test class")
            end
          end
        end
      else
        ok = true
        if !(entries["src"] && entries["test"])
          add_error("The archive does not contain src/ and test/ subdirectories")
          ok = false
        end
        if ok
          self.test_class.split.each do |tc|
            next if (tc.starts_with? "-" || (Float(tc) rescue false))
            if !entries["test"]["#{tc}.java"]
              add_error("There is no #{tc}.java file to match the specified test class")
            end
          end
        end
      end      
    rescue Exception => e
      e_msg = e.to_s
      e_msg = e_msg.dump[1...-1] unless e_msg.is_utf8?
      add_error("Could not read upload: #{e_msg}")
    end
  end
end
