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

  def export_data
    export_tap_data
  end
  def export_data_schema
    "tap_style_export_schema"
  end
  def import_data(who_grades, file)
    import_tap_data(who_grades, file)
  end
  def import_data_schema
    "tap_style_import_schema"
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
    [
      "style.tap",
      {"XDG_RUNTIME_DIR" => nil},
      ["racket", Rails.root.join("lib/assets/checkstyle.rkt").to_s,
       "--max-points", self.avail_score.to_s,
       "--line-width", self.line_length.to_s,
       @tmpdir],
      [[@tmpdir, Upload.upload_path_for(sub.upload.extracted_path.to_s)]].to_h
    ]
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

  @@resource_files = {
    "lib/assets/checkstyle.rkt": ["checkstyle-rkt", "scheme", false],
    "lib/assets/check-spacing.rkt": ["check-spacing-rkt", "scheme", false],
    "lib/assets/linter.rkt": ["linter-rkt", "scheme", false],
    "lib/assets/render-racket.rkt": ["render-racket-rkt", "scheme", false],
    "lib/assets/retab.rkt": ["retab-rkt", "scheme", false],
  }

  def generate_files_hash(sub)
    files = {
      submission: {
        url: sub.upload.url,
        mime_type: sub.upload.read_metadata[:mimetype],
        should_replace_paths: false
      }
    }

    @@resource_files.each do |file_path, (files_key, mime, should_replace_paths)|
      files[files_key] = {
        url: "#{Settings['site_url']}/resources/#{file_path.to_s.gsub('lib/assets/', '')}",
        mime_type: mime,
        should_replace_paths: should_replace_paths
      }
    end
    files
  end

  def get_grading_script(sub)
    build_script_str = File.read(Rails.root.join('lib/assets/orca-grading-scripts/racket_style_grader.json'))
    build_script_str.gsub!("$MAX_POINTS", self.avail_score.to_s)
    build_script_str.gsub!("$LINE_WIDTH", self.line_length.to_s)
    JSON.parse(build_script_str)
  end


  def self.dockerfile_path
    Rails.root.join 'lib/assets/dockerfiles/racket-grader.Dockerfile'
  end
  def self.dockerfile_sha_sum
    Digest::SHA256.hexdigest(File.read(RacketStyleGrader.dockerfile_path))
  end
  def dockerfile_sha_sum
    RacketStyleGrader.dockerfile_sha_sum
  end
  def dockerfile_path
    RacketStyleGrader.dockerfile_path
  end

end
