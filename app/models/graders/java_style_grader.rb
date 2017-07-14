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
    run_command_produce_tap assignment, sub
  end
  def get_command_arguments(assignment, sub)
    files_dir = sub.upload.extracted_path
    if self.upload and self.upload.submission_path and File.file?(self.upload.submission_path)
      [
        "style.tap",
        {},
        ["java", "-jar", Rails.root.join("lib/assets/StyleChecker.jar").to_s,
         files_dir.to_s,
         "+config", self.upload.submission_path.to_s,
         "-maxPoints", self.avail_score.to_s]
      ]
    else
      [
        "style.tap",
        {},
        ["java", "-jar", Rails.root.join("lib/assets/StyleChecker.jar").to_s,
         files_dir.to_s, "-maxPoints", self.avail_score.to_s]
      ]
    end
  end
end
