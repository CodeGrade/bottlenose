require 'open3'
require 'tap_parser'
require 'audit'

class Grader < ApplicationRecord
  class GradingJob
    # This job class helps keeps track of all jobs that go into the beanstalk system
    # It keeps track of starting times for them, and the arguments passed in.
    # It prunes dead jobs every now and then (250jobs, by default), or whenever
    # the status page is visited.
    # Change this one job class to change the integration with beanstalk.
    include Backburner::Queue
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
      Grader.find(grader_id).grade_sync!(assn_id, sub_id)
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
      end
    end
  end

  belongs_to :submission
  belongs_to :assignment
  belongs_to :upload
  belongs_to :extra_upload, class_name: 'Upload'

  has_many :grades
  validates_presence_of :assignment
  validates :order, presence: true, uniqueness: {scope: :assignment_id}

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
  attr_accessor :test_class
  attr_accessor :errors_to_show
  # Needed to make the upload not be anonymous
  attr_accessor :upload_by_user_id

  def self.unique
    select(column_names - ["id"]).distinct
  end

  def grade_sync!(asn_id, sub_id)
    assignment = Assignment.find(asn_id)
    submission = Submission.find(sub_id)

    ans = do_grading(assignment, submission)
    submission.compute_grade! if submission.grade_complete?
    ans
  end

  def grade(assignment, submission, prio = 0)
    if autograde?
      GradingJob.enqueue(self, assignment, submission,
                         prio: 1000 + prio, ttr: 1200)
    else
      do_grading(assignment, submission)
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

  def extra_upload_file=(data)
    if data.nil?
      return
    end

    up = Upload.new
    up.user = User.find_by(id: self.upload_by_user_id)
    up.store_upload!(data, {
                       type: "#{type} Extra Configuration",
                       date: Time.now.strftime("%Y/%b/%d %H:%M:%S %Z"),
                       mimetype: data.content_type
                     })

    unless up.save
      self.errors.add(:upload_file, "could not save upload")
      return
    end
    self.extra_upload_id = up.id
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
