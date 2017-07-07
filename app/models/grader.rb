require 'open3'
require 'tap_parser'
require 'audit'

class Grader < ApplicationRecord
  belongs_to :submission
  belongs_to :assignment
  belongs_to :upload
  has_many :grades
  validates_presence_of :assignment
  validates :order, presence: true, uniqueness: {scope: :assignment_id}

  # Needed because when Cocoon instantiates new graders, it doesn't know what
  # subtype they are, yet
  attr_accessor :test_class
  attr_accessor :errors_to_show
  # Needed to make the upload not be anonymous
  attr_accessor :upload_by_user_id

  def self.unique
    select(column_names - ["id"]).distinct
  end

  def grade(assignment, submission)
    ans = do_grading(assignment, submission)
    submission.compute_grade! if submission.grade_complete?
    ans
  end

  def autograde?
    false
  end

  def autograde!(assignment, submission)
    grade(assignment, submission)
  end

  def grade_exists_for(sub)
    !Grade.find_by(grader_id: self.id, submission_id: sub.id).nil?
  end

  def ensure_grade_exists_for!(sub)
    g = grade_for(sub)
    if g.new_record?
      g.save
      true
    else
      false
    end
  end

  def upload_file
    if @upload_data
      @upload_data.original_filename
    elsif self.upload
      self.upload.file_name
    else
      nil
    end
  end

  def upload_file=(data)
    if data.nil?
      errors.add(:base, "You need to submit a file.")
      return
    end

    up = Upload.new
    up.user = User.find_by(id: self.upload_by_user_id)
    up.store_upload!(data, {
                       type: "#{type} Configuration",
                       date: Time.now.strftime("%Y/%b/%d %H:%M:%S %Z"),
                       mimetype: data.content_type
                     })

    unless up.save
      self.errors.add(:upload_file, "could not save upload")
      return
    end
    self.upload_id = up.id
  end

  def assign_attributes(attrs)
    self.upload_by_user_id = attrs[:upload_by_user_id]
    super(attrs)
  end
  
  protected

  def do_grading(assignment, submission)
    fail NotImplementedError, "Each grader should implement this"
  end

  def grade_for(sub)
    g = Grade.find_or_initialize_by(grader_id: self.id, submission_id: sub.id)
    if g.new_record?
      g.out_of = self.avail_score
      g.save
    end
    g
  end

  def run_command_produce_tap(assignment, sub)
    g = self.grade_for sub
    u = sub.upload
    grader_dir = u.grader_path(g)

    grader_dir.mkpath

    prefix = "Assignment #{assignment.id}, submission #{sub.id}"

    tap_out, env, args = get_command_arguments(assignment, sub)

    Audit.log("#{prefix}: Running #{self.type}.  Command line: #{args.join(' ')}\n")
    print("#{prefix}: Running #{self.type}.  Command line: #{args.join(' ')}\n")
    output, err, status = Open3.capture3(env, *args)
    File.open(grader_dir.join(tap_out), "w") do |style|
      style.write(Upload.upload_path_for(output))
      g.grading_output = style.path
    end
    if !status.success?
      Audit.log "#{prefix}: #{self.type} checker failed: status #{status}, error: #{err}\n"
      InlineComment.where(submission: sub, grade: g).destroy_all
      
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
        grade: g,
        user: nil,
        label: "general",
        severity: InlineComment::severities["error"],
        weight: self.avail_score,
        comment: "Could not parse your program, so could not compute any style points at all",
        suppressed: false)

      return 0
    else
      tap = TapParser.new(output)
      Audit.log "#{prefix}: #{self.type} checker results: Tap: #{tap.points_earned}\n"

      g.score = tap.points_earned
      g.out_of = tap.points_available
      g.updated_at = DateTime.now
      g.available = true
      g.save!

      upload_inline_comments(tap, sub)

      return self.avail_score.to_f * (tap.points_earned.to_f / tap.points_available.to_f)
    end
  end

  def get_command_arguments(assignment, sub)
    fail NotImplementedError, "Must implement this if you're using run_command_produce_tap"
  end

  def upload_inline_comments(tap, sub)
    g = self.grade_for sub
    InlineComment.where(submission: sub, grade: g).destroy_all
    ics = tap.tests.map do |t|
      InlineComment.new(
        submission: sub,
        title: t[:comment],
        filename: t[:info]["filename"],
        line: t[:info]["line"],
        grade: g,
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
