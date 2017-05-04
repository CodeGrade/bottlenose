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
    self.upload.extract_contents! if self.upload
    g = self.grader_for sub
    u = sub.upload
    u.extract_contents!
    files_dir = u.extracted_path
    grader_dir = u.grader_path(g)

    grader_dir.mkpath

    prefix = "Assignment #{assignment.id}, submission #{sub.id}"

    if self.upload and self.upload.submission_path and File.file?(self.upload.submission_path)
      Audit.log("#{prefix}: Running JavaStyle checker.  Command line: java -jar #{Rails.root.join('lib/assets/StyleChecker.jar').to_s} #{files_dir.to_s} +config #{self.upload.submission_path} -maxPoints #{self.avail_score.to_s}\n")
      output, err, status = Open3.capture3("java", "-jar", Rails.root.join("lib/assets/StyleChecker.jar").to_s,
                                           files_dir.to_s,
                                           "+config", self.upload.submission_path.to_s,
                                           "-maxPoints", self.avail_score.to_s)
    else
      Audit.log("#{prefix}: Running JavaStyle checker.  Command line: java -jar #{Rails.root.join('lib/assets/StyleChecker.jar').to_s} #{files_dir.to_s} -maxPoints #{self.avail_score.to_s}\n")
      output, err, status = Open3.capture3("java", "-jar", Rails.root.join("lib/assets/StyleChecker.jar").to_s,
                                           files_dir.to_s, "-maxPoints", self.avail_score.to_s)
    end
    File.open(grader_dir.join("style.tap"), "w") do |style|
      style.write(Upload.upload_path_for(output))
      g.grading_output = style.path
    end
    if !status.success?
      Audit.log "#{prefix}: JavaStyle checker failed: status #{status}, error: #{err}\n"
      InlineComment.where(submission: sub, grader: g).destroy_all
      
      g.score = 0
      g.out_of = self.avail_score
      g.updated_at = DateTime.now
      g.available = true
      g.save!

      InlineComment.create!(
        submission: sub,
        title: "Compilation errors",
        filename: sub.upload.extracted_path.to_s,
        line: 0,
        grader: g,
        user: nil,
        label: "general",
        severity: InlineComment::severities["error"],
        weight: self.avail_score,
        comment: "Could not parse your program, so could not compute any style points at all",
        suppressed: false)

      return 0
    else
      tap = TapParser.new(output)
      Audit.log "#{prefix}: JavaStyle checker results: Tap: #{tap.points_earned}\n"

      g.score = tap.points_earned
      g.out_of = tap.points_available
      g.updated_at = DateTime.now
      g.available = true
      g.save!

      upload_inline_comments(tap, sub)

      return self.avail_score.to_f * (tap.points_earned.to_f / tap.points_available.to_f)
    end
  end
  
  def upload_inline_comments(tap, sub)
    g = self.grader_for sub
    InlineComment.where(submission: sub, grader: g).destroy_all
    ics = tap.tests.map do |t|
      InlineComment.new(
        submission: sub,
        title: t[:comment],
        filename: t[:info]["filename"],
        line: t[:info]["line"],
        grader: g,
        user: nil,
        label: t[:info]["category"],
        severity: InlineComment::severities[t[:info]["severity"].humanize(:capitalize => false)],
        comment: t[:info]["message"],
        weight: t[:info]["weight"],
        suppressed: t[:info]["suppressed"])
    end
    InlineComment.import ics
  end

end
