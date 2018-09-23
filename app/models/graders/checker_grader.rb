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
    "#{self.avail_score} points: Run Checker tests in #{test_class} from #{filename}, " +
      "and show #{pluralize(errors_to_show, 'failed test')}"
  end

  protected
  def load_checker_params
    return if new_record?
    testClass, errorsToShow = self.params.to_s.split(";")
    self.test_class = testClass
    self.errors_to_show = errorsToShow.to_i
  end
  def set_checker_params
    self.params = "#{self.test_class};#{self.errors_to_show}"
  end
    
  def copy_srctest_from_to(from, to, prefix = "")
    if !prefix.empty?
      prefix = "#{prefix}: "
    end
    if to.kind_of? String
      to = Pathname.new(to)
    end
    if from.kind_of? String
      from = Pathname.new(from)
    end
    # preserves when the submission contains src/ and test/ directories
    # or creates them if only sources are submitted
    # to.join("src").mkpath
    # to.join("test").mkpath
    
    if Dir.exists?(from.join("src")) && Dir.exists?(from.join("test"))
      Audit.log("#{prefix}From = #{from} and contains src/ and test/")
      # FileUtils.cp_r("#{from.join('src')}/.", "#{to.join('src')}/")
      # FileUtils.cp_r("#{from.join('test')}/.", "#{to.join('test')}/")
      FileUtils.cp_r("#{from.join('src')}/.", "#{to}")
      FileUtils.cp_r("#{from.join('test')}/.", "#{to}")
    else
      Audit.log("#{prefix}From = #{from} and does not contain src/ and test/")
      # FileUtils.cp_r("#{from}/.", "#{to.join('src')}/")
      FileUtils.cp_r("#{from}/.", "#{to}/")
    end
  end

  def do_grading(assignment, sub)
    g = self.grade_for sub
    u = sub.upload
    grader_dir = u.grader_path(g)

    grader_dir.mkpath

    assets_dir = Rails.root.join('lib/assets')
    prefix = "Assignment #{assignment.id}, submission #{sub.id}"
    
    File.open(grader_dir.join("checker.tap"), "w") do |checker|
      File.open(grader_dir.join("details.log"), "w") do |details|
        Dir.mktmpdir("grade-#{sub.id}-#{g.id}") do |build_dir|
          Headless.ly(display: g.id % Headless::MAX_DISPLAY_NUMBER, autopick: true) do
          # build_dir = grader_dir.join("build")
          # build_dir.mkpath
            Audit.log("#{prefix}: Grading in #{build_dir}")
            if (Dir.exists?(self.upload.extracted_path.join("starter")) &&
                Dir.exists?(self.upload.extracted_path.join("testing")))
              copy_srctest_from_to(self.upload.extracted_path.join("starter"), build_dir, prefix)
            end
            copy_srctest_from_to(u.extracted_path, build_dir, prefix)
            FileUtils.cp("#{assets_dir}/tester-2.jar", build_dir)
            FileUtils.cp("#{assets_dir}/javalib.jar", build_dir)
            if (Dir.exists?(self.upload.extracted_path.join("starter")) &&
                Dir.exists?(self.upload.extracted_path.join("testing")))
              copy_srctest_from_to(self.upload.extracted_path.join("testing"), build_dir, prefix)
            else
              copy_srctest_from_to(self.upload.extracted_path, build_dir, prefix)
            end
            details.write "Contents of temp directory are:\n"
            output, status = Open3.capture2("ls", "-R", build_dir.to_s)
            details.write output

            classpath = "tester-2.jar:javalib.jar:.:./*"

            any_problems = false
            Dir.glob("#{build_dir}/**/*.java").each do |file|
              next if Pathname.new(file).ascend.any? {|c| c.basename.to_s == "__MACOSX" || c.basename.to_s == ".DS_STORE" }
              Audit.log "#{prefix}: Compiling #{file}"
              comp_out, comp_err, comp_status, timed_out =
                                               ApplicationHelper.capture3("javac", "-cp", classpath, file,
                                                                          *Dir.glob("#{build_dir}/*.java"),
                                                                          chdir: build_dir.to_s,
                                                                          timeout: Grader::DEFAULT_COMPILE_TIMEOUT)
              details.write("Compiling #{file}: (exit status #{comp_status})\n")
              details.write(comp_out)
              if timed_out
                details.write("Compiling #{file}: Compilation timed out after #{Grader::DEFAULT_COMPILE_TIMEOUT} seconds\n")
              end
              if timed_out || !comp_status&.success?
                details.write("Errors building student code:\n")
                details.write(comp_err)
                Audit.log("#{prefix}: #{file} failed with compilation errors; see details.log")
                any_problems = true
              end
            end

            # details.write "Contents of temp directory are:\n"
            # output, status = Open3.capture2("ls", "-R", build_dir.to_s)
            # details.write output

            Audit.log("#{prefix}: Running Checker with timeout of #{Grader::DEFAULT_GRADING_TIMEOUT}")
            test_out, test_err, test_status, timed_out =
                                             ApplicationHelper.capture3("java", "-XX:MaxJavaStackTraceDepth=1000000",
                                                                        "-cp", classpath, "tester.Main",
                                                                        "-secmon", "-tap", "-enforceTimeouts",
                                                                        self.test_class,
                                                                        chdir: build_dir.to_s,
                                                                        timeout: Grader::DEFAULT_GRADING_TIMEOUT)
            details.write("Checker output: (exit status #{test_status})\n")
            details.write(test_out)
            if timed_out
              details.write("Running tests timed out after #{Grader::DEFAULT_GRADING_TIMEOUT} seconds")
            end
            if timed_out || !test_status&.success?
              details.write("Checker errors:\n")
              details.write(test_err)
              Audit.log("#{prefix}: Checker failed with errors; see details.log")
              any_problems = true
            end

            if any_problems
              g.grading_output_path = details.path
              g.score = 0
              g.out_of = self.avail_score

              g.updated_at = DateTime.now
              g.available = true
              g.save!

              Audit.log("#{prefix}: Errors prevented grading; giving a 0")
              return 0
            else
              begin
                checker.write(test_out)
                g.grading_output_path = checker.path

                tap = TapParser.new(test_out)
                g.score = tap.points_earned
                g.out_of = tap.points_available
                g.updated_at = DateTime.now
                g.available = true
                g.save!

                Audit.log("#{prefix}: Checker gives raw score of #{g.score} / #{g.out_of}")
                return self.avail_score.to_f * (tap.points_earned.to_f / tap.points_available.to_f)
              rescue Exception
                g.grading_output_path = details.path
                g.score = 0
                g.out_of = self.avail_score
                g.updated_at = DateTime.now
                g.available = true
                g.save!
                Audit.log("#{prefix}: Errors prevented grading; giving a 0")
                return 0
              end
            end
          end
        end
      end
    end
    return 0
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
    end
    return if self.upload.nil? || self.test_class.blank?
    begin
      entries = self.upload.upload_entries
      if entries.size == 1 && entries.keys.any?{|k| k.ends_with? ".java"}
        if !self.upload.upload_data.include? "class #{self.test_class}"
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
          if !entries["testing"]["test"]["#{self.test_class}.java"]
            add_error("There is no #{self.test_class}.java file to match the specified test class")
          end
        end
      else
        ok = true
        if !(entries["src"] && entries["test"])
          add_error("The archive does not contain src/ and test/ subdirectories")
          ok = false
        end
        if ok
          if !entries["test"]["#{self.test_class}.java"]
            add_error("There is no #{self.test_class}.java file to match the specified test class")
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
