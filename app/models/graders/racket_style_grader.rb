require 'open3'
require 'tap_parser'
require 'audit'
require 'headless'

class RacketStyleGrader < Grader
  def autograde?
    true
  end

  def display_type
    "Racket Style"
  end
  
  def to_s
    if self.upload
      "#{self.avail_score} points: Run Racket style checker, using #{self.upload.file_name}"
    else
      "#{self.avail_score} points: Run Racket style checker"
    end
  end

  after_initialize :load_style_params
  before_validation :set_style_params

  def assign_attributes(attrs)
    super
    set_style_params
  end
  
  protected
  
  def do_grading(assignment, sub)
    Headless.ly do
      sub.upload.extract_contents_to!(nil, sub.upload.extracted_path, false)
      run_command_produce_tap assignment, sub
      sub.upload.extract_contents_to!(nil, sub.upload.extracted_path, true)
    end
  end

  def get_command_arguments(assignment, sub)
    [
      "style.tap",
      {"XDG_RUNTIME_DIR" => nil},
      ["racket", Rails.root.join("lib/assets/checkstyle.rkt").to_s,
       "--max-points", self.avail_score.to_s,
       "--line-width", self.line_length.to_s,
       sub.upload.extracted_path.to_s]
    ]
  end

  def load_style_params
    return if new_record?
    self.line_length = self.params.to_i
  end

  def set_style_params
    self.params = "#{self.line_length}"
  end
end
