require 'securerandom'
require 'audit'

class Assignment < ApplicationRecord
  attr_accessor :removefile

  belongs_to :blame, :class_name => "User", :foreign_key => "blame_id"

  belongs_to :course

  belongs_to :lateness_config
  accepts_nested_attributes_for :lateness_config

  belongs_to :related_assignment, :class_name => "Assignment", :foreign_key => "related_assignment_id"

  has_many :submissions, :dependent => :restrict_with_error
  has_many :used_subs, :dependent => :destroy

  has_many :assignment_graders, :dependent => :destroy
  has_many :graders, through: :assignment_graders
  accepts_nested_attributes_for :graders, allow_destroy: true
  validates :graders, presence: true
  validates_associated :graders

  validates :name,      :uniqueness => { :scope => :course_id }
  validates :name,      :presence => true
  validates :course_id, :presence => true
  validates :due_date,  :presence => true
  validates :available, :presence => true
  validates :blame_id,  :presence => true
  validates :points_available, :numericality => true
  validates :lateness_config, :presence => true

  def sub_late?(sub)
    self.lateness_config.late?(self, sub)
  end

  def sub_days_late(sub, raw = false)
    self.lateness_config.days_late(self, sub, raw)
  end

  def sub_late_penalty(sub)
    self.lateness_config.late_penalty(self, sub)
  end

  def sub_allow_submission?(sub)
    self.lateness_config.allow_submission?(self, sub)
  end

  def rate_limit?(sub)
    if self.max_attempts.to_i > 0 and self.submissions.count >= self.max_attempts.to_i
      "permanent"
    elsif self.rate_per_hour.to_i > 0 and
         self.submission.where('created_at >= ?', DateTime.now - 1.hour).count > self.rate_per_hour.to_i
      "temporary"
    else
      false
    end
  end

  def assignment_upload
    Upload.find_by_id(assignment_upload_id)
  end

  def assignment_file
    if assignment_upload_id.nil?
      ""
    else
      assignment_upload.file_name
    end
  end

  def assignment_file_name
    assignment_file
  end

  def assignment_full_path
    assignment_upload.submission_path
  end

  def assignment_file_path
    if assignment_upload_id.nil?
      ""
    else
      assignment_upload.path
    end
  end
  def assignment_file=(data)
    @assignment_file_data = data
  end

  def save_uploads!
    user = User.find(blame_id)

    if @assignment_file_data.nil?
      return
    else
      unless assignment_upload_id.nil?
        Audit.log("Assn #{id}: Orphaning assignment upload " +
                  "#{assignment_upload_id} (#{assignment_upload.secret_key})")
      end

      up = Upload.new
      up.user_id = user.id
      up.store_upload!(@assignment_file_data, {
        type:       "Assignment File",
        user:       "#{user.name} (#{user.id})",
        course:     "#{course.name} (#{course.id})",
        date:       Time.now.strftime("%Y/%b/%d %H:%M:%S %Z"),
        mimetype:   @assignment_file_data.content_type
      })
      up.save!

      self.assignment_upload_id = up.id
      self.save!

      Audit.log("Assn #{id}: New assignment file upload by #{user.name} " +
                "(#{user.id}) with key #{up.secret_key}")
    end
  end

  def submissions_for(user)
    Assignment.submissions_for([user], [self])
  end

  def self.submissions_for(users, assns)
    team_assns, solo_assns = assns.to_a.partition(&:team_subs?)
    user_ids = users.map(&:id)
    solo_subs = Submission
                .where(user_id: user_ids)
                .where(assignment_id: solo_assns.map(&:id))
                .select("submissions.*", "submissions.user_id AS for_user")
    team_subs = Submission
                .joins(:team)
                .joins("JOIN team_users ON team_users.team_id = teams.id")
                .where("team_users.user_id": user_ids).where("submissions.assignment_id": team_assns.map(&:id))
                .select("submissions.*", "team_users.user_id AS for_user")
    Submission.from("(#{solo_subs.to_sql} UNION #{team_subs.to_sql}) AS submissions")
      .order(created_at: :desc)
  end

  def used_submissions
    # Only those unique submissions that are marked as used-for-grading for this assignment
    all_used_subs.distinct
  end

  def all_used_subs
    Submission.joins(:used_subs).where(assignment_id: self.id)
  end

  def used_sub_for(user)
    ans = UsedSub.find_by(user_id: user.id, assignment_id: self.id)
    if ans.nil?
      ans
    else
      ans.submission
    end
  end

  def main_submissions
    used_subs.map do |sfg|
      sfg.submission
    end
  end

  def graders_ordered
    graders.order("assignment_graders.order")
  end
end
