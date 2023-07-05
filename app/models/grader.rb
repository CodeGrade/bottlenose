require 'open3'
require 'tap_parser'
require 'audit'
require 'headless'

# NOTE: This is an utter hack, designed to work around an infelicity in
# the Headless implementation: if Xvfb claims it's running on some port but
# is refusing connections, then Headless::ensure_xvfb_launched will abort with
# an exception, even if other displays are potentially available.
# This monkey-patch implements https://github.com/leonid-shevtsov/headless/issues/100
# and hopefully can be removed one day...
class Headless
  private
  def pick_available_display(display_set, can_reuse)
    @error = nil
    display_set.each do |display_number|
      @display = display_number

      return true if xvfb_running? && can_reuse && (xvfb_mine? || !@autopick_display)
      begin
        return true if !xvfb_running? && launch_xvfb
      rescue Headless::Exception => e
        @error = e
      end
    end
    raise @error || Headless::Exception.new("Could not find an available display")
  end
end


class Grader < ApplicationRecord
  DEFAULT_COMPILE_TIMEOUT = 60
  DEFAULT_GRADING_TIMEOUT = 300
  DEFAULT_TEST_TIMEOUT    = 10
  DEFAULT_ERRORS_TO_SHOW   = 3
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
        sub: sub.id,
        opts: opts
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

  belongs_to :assignment
  belongs_to :upload, optional: true
  belongs_to :extra_upload, class_name: 'Upload', optional: true

  has_many :grades
  validates :assignment, presence: true
  validates :order, presence: true, uniqueness: {scope: :assignment_id}
  before_save :recompute_grades, if: :avail_score_changed?
  before_save :save_uploads

  include GradersHelper
  
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

  def generate_grading_job(sub)
    fail NotImplementedError, "Graders who send jobs to Orca should implement this method."
  end

  def generate_grading_job_metadata_table(sub)
    ans = {}
    if sub.team
      ans["display_name"] = sub.team.member_names
      ans["id"] = sub.team_id
      ans["user_or_team"] = "team"
    else
      ans["display_name"] = sub.user.display_name
      ans["id"] = sub.user_id
      ans["user_or_team"] = "user"
    end
    ans
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
  def test_timeout
    @test_timeout
  end
  def test_timeout=(value)
    params_will_change! if test_timeout != value
    @test_timeout = value
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

  # NOTE: These two methods may be overridden if a single grader contains
  # both regular and E.C. weight
  def normal_weight
    if self.extra_credit
      0.0
    else
      self.avail_score.to_f
    end
  end

  def extra_credit_weight
    if self.extra_credit
      self.avail_score.to_f
    else
      0.0
    end
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
      mimetype: data.content_type,
      prof_override: {file_count: true, file_size: true}
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
      mimetype: data.content_type,
      prof_override: {file_count: true, file_size: true}
    }
    self.extra_upload = up
    self.extra_upload_id_will_change!
  end

  def assign_attributes(attrs)
    if attrs[:removefile] == "remove"
      self.upload = nil
    end
    attrs.delete :removefile
    self.upload_by_user_id = attrs[:upload_by_user_id]
    self.assignment = attrs[:assignment] if self.assignment.nil? && attrs[:assignment]
    super(attrs)
  end

  def export_data
    # Export all the data from this grader
    fail NotImplementedError, "Each grader should implement this"
  end

  def export_data_schema
    # Describe the format for export_data
    # Return either and html_safe? string, or the name of a partial
    fail NotImplementedError, "Each grader should implement this"
  end

  def import_data
    # Import all the data (in the same format as produced by export_data)
    fail NotImplementedError, "Each grader should implement this"
  end

  def import_data_schema
    # Describe the format for import_data
    # Return either and html_safe? string, or the name of a partial
    fail NotImplementedError, "Each grader should implement this"
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
 
  def is_int(v)
    Integer(v) rescue false
  end
  def is_float(v)
    Float(v) rescue false
  end
  def add_error(msg)
    order = self.assignment.graders.sort_by(&:order).index(self)
    self.errors.add("##{order + 1}", msg)
  end
end
