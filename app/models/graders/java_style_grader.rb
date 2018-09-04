require 'open3'
require 'tap_parser'
require 'audit'

class JavaStyleGrader < Grader
  def autograde?
    true
  end

  def display_type
    "Java Style"
  end
  
  def to_s
    if self.upload
      "#{self.avail_score} points: Run Java style checker, using #{self.upload.file_name}"
    else
      "#{self.avail_score} points: Run Java style checker"
    end
  end

  protected
  
  def do_grading(assignment, sub)
    run_command_produce_tap assignment, sub, timeout: Grader::DEFAULT_GRADING_TIMEOUT
  end
  def get_command_arguments(assignment, sub)
    files_dir = sub.upload.extracted_path
    ans = [
      "style.tap",
      {},
      ["java", "-jar", Rails.root.join("lib/assets/StyleChecker.jar").to_s,
       files_dir.to_s,
       "+pmdAddClasspath", Rails.root.join("lib/assets/tester-2.jar").to_s,
       "+pmdAddClasspath", Rails.root.join("lib/assets/javalib.jar").to_s,
       "-maxPoints", self.avail_score.to_s],
      [[files_dir.to_s, Upload.upload_path_for(files_dir.to_s)]].to_h
    ]
    if self.upload&.submission_path && File.file?(self.upload.submission_path)
      ans[2].insert(4, 
         "+config", self.upload.submission_path.to_s)
    end
    ans
  end

  def recompute_grades
    # nothing to do:
    # we already compute the score here based on the TAP output
  end
end
