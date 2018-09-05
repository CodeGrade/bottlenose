require 'open3'
require 'tap_parser'
require 'audit'

class Grader < ApplicationRecord
  DEFAULT_COMPILE_TIMEOUT = 60
  DEFAULT_GRADING_TIMEOUT = 300
  class GradingJob
    # This job class helps keeps track of all jobs that go into the beanstalk system
    # It keeps track of starting times for them, and the arguments passed in.
    # It prunes dead jobs every now and then (250jobs, by default), or whenever
    # the status page is visited.
    # Change this one job class to change the integration with beanstalk.
    include Backburner::Queue
    # With a Postgres connection limit of 100, and a pool size of 5,
    # this needs to stay low enough to leave some headroom, and 5 * 15 == 75 < 100
    queue_jobs_limit 15
    def self.enqueue(grader, assn, sub, opts = {})
      job = Backburner::Worker.enqueue GradingJob, [grader.id, assn.id, sub.id], opts

      Grader.delayed_grades[job[:id]] = {
        start_time: Time.now,
        grader_type: grader.display_type,
        user_name: sub.user.display_name,
        course: assn.course.id,
        assn: assn.id,
        sub: sub.id
      }
      job[:id]
    end
    def self.perform(grader_id, assn_id, sub_id)
      Grader.find(grader_id).grade_sync!(Assignment.find(assn_id), Submission.find(sub_id))
    end

    def self.prune(threshold = 250)
      begin
        job_ids = Grader.delayed_grades.keys
        return if job_ids.count < threshold
        bean = Backburner::Connection.new(Backburner.configuration.beanstalk_url)
        job_ids.each do |k|
          job = bean.jobs.find(k)
          if job.nil?
            Grader.delayed_grades.delete k
          elsif job.stats["state"] == "buried"
            Grader.delayed_grades.delete k
            job.delete
          end
        end
      rescue
        # nothing to do
      ensure
        bean.close if bean
      end
    end
    def self.clear_all!
      begin
        count = 0
        bean = Backburner::Connection.new(Backburner.configuration.beanstalk_url)
        bean.tubes.each do |tube|
          [:ready, :delayed, :buried].each do |status|
            while tube.peek(status)
              tube.peek(status).delete
              count += 1
            end
          end
        end
        Grader.delayed_grades.clear
        return count
      rescue Exception => e
        return "Error clearing queue: #{e}"
      ensure
        bean.close if bean
      end
    end
  end

  belongs_to :submission
  belongs_to :assignment
  belongs_to :upload
  belongs_to :extra_upload, class_name: 'Upload'

  has_many :grades
  validates :assignment, presence: true
  validates :order, presence: true, uniqueness: {scope: :assignment_id}
  before_save :recompute_grades, if: :avail_score_changed?
  before_save :save_uploads
  
  class << self
    attr_accessor :delayed_grades
    attr_accessor :delayed_count
  end
  @delayed_grades = {}
  @delayed_count = 0
  
  def self.delayed_grades
    @delayed_grades
  end
  def self.delayed_count
    @delayed_count
  end

  # Needed because when Cocoon instantiates new graders, it doesn't know what
  # subtype they are, yet
  def test_class
    @test_class
  end
  def test_class=(value)
    params_will_change! if test_class != value
    @test_class = value
  end
  def errors_to_show
    @errors_to_show
  end
  def errors_to_show=(value)
    params_will_change! if errors_to_show != value
    @errors_to_show = value
  end
  def self.default_line_length
    102
  end
  def line_length
    (@line_length || Grader.default_line_length).to_i
  end
  def line_length=(value)
    params_will_change! if line_length != value
    @line_length = value
  end

  # Needed to make the upload not be anonymous
  attr_accessor :upload_by_user_id

  def self.unique
    select(column_names - ["id"]).distinct
  end

  def grade_sync!(assignment, submission)
    ans = do_grading(assignment, submission)
    submission.compute_grade! if submission.grade_complete?
    ans
  end

  def grade(assignment, submission, prio = 0)
    if autograde?
      GradingJob.enqueue self, assignment, submission, prio: 1000 + prio, ttr: 1200
    else
      grade_sync!(assignment, submission)
    end
  end

  def autograde?
    false
  end

  def autograde!(assignment, submission, prio = 0)
    if autograde?
      grade(assignment, submission, prio)
    end
  end

  def grade_exists_for(sub)
    !Grade.find_by(grader_id: self.id, submission_id: sub.id).nil?
  end

  def ensure_grade_exists_for!(sub)
    g = grade_for(sub, true)
    if g.new_record?
      g.save
      true
    else
      false
    end
  end

  def guess_who_graded(sub)
    nil
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

  def extra_upload_file
    self.extra_upload&.file_name
  end

  def upload_file=(data)
    if data.nil?
      errors.add(:base, "You need to submit a file.")
      return
    end

    up = Upload.new
    up.user = User.find_by(id: self.upload_by_user_id)
    up.assignment = assignment
    up.upload_data = data
    up.metadata = {
      type: "#{type} Configuration",
      date: Time.now.strftime("%Y/%b/%d %H:%M:%S %Z"),
      mimetype: data.content_type
    }
    self.upload = up
    self.upload_id_will_change!
  end

  def extra_upload_file=(data)
    if data.nil?
      return
    end

    up = Upload.new
    up.user = User.find_by(id: self.upload_by_user_id)
    up.assignment = assignment
    up.upload_data = data
    up.metadata = {
      type: "#{type} Extra Configuration",
      date: Time.now.strftime("%Y/%b/%d %H:%M:%S %Z"),
      mimetype: data.content_type
    }
    self.extra_upload = up
    self.extra_upload_id_will_change!
  end

  def assign_attributes(attrs)
    self.upload_by_user_id = attrs[:upload_by_user_id]
    self.assignment = attrs[:assignment] if self.assignment.nil? && attrs[:assignment]
    super(attrs)
  end

  protected

  def save_uploads
    self.upload&.save!
    self.extra_upload&.save!
  end
  
  def recompute_grades
    # nothing to do by default
  end

  def do_grading(assignment, submission)
    fail NotImplementedError, "Each grader should implement this"
  end

  def grade_for(sub, nosave = false)
    g = Grade.find_or_initialize_by(grader_id: self.id, submission_id: sub.id)
    if g.new_record?
      g.out_of = self.avail_score
      g.save unless nosave
    end
    g
  end

  def run_command_produce_tap(assignment, sub, timeout: nil)
    g = self.grade_for sub
    u = sub.upload
    grader_dir = u.grader_path(g)

    grader_dir.mkpath

    prefix = "Assignment #{assignment.id}, submission #{sub.id}"

    tap_out, env, args, replacements = get_command_arguments(assignment, sub)

    Audit.log("#{prefix}: Running #{self.type}.  Extracted dir: #{u.extracted_path}.  Timeout: #{timeout || 'unlimited'}.  Command line: #{args.join(' ')}")
    print("#{prefix}: Running #{self.type}.  Extracted dir: #{u.extracted_path}.  Timeout: #{timeout || 'unlimited'}.  Command line: #{args.join(' ')}\n")
    output, err, status, timed_out = ApplicationHelper.capture3(env, *args, timeout: timeout)
    File.open(grader_dir.join(tap_out), "w") do |style|
      replacements&.each do |rep, with|
        output = output.gsub(rep, with)
      end
      style.write(Upload.upload_path_for(output))
      g.grading_output = style.path
    end
    if timed_out || !status&.success?
      Audit.log "#{prefix}: #{self.type} checker failed: timed out #{timed_out}, status #{status}, error: #{err}"
      InlineComment.where(submission: sub, grade: g).destroy_all

      g.score = 0
      g.out_of = self.avail_score
      g.updated_at = DateTime.now
      g.available = true
      g.save!

      InlineComment.create!(
        submission: sub,
        title: "Compilation errors",
        filename: Upload.upload_path_for(sub.upload.extracted_path.to_s),
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
      Audit.log "#{prefix}: #{self.type} checker results: Tap: #{tap.points_earned}"

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
      puts "Severity is #{t[:info]}"
      InlineComment.new(
        submission: sub,
        title: t[:comment],
        filename: Upload.upload_path_for(t[:info]["filename"]),
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
