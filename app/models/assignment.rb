require 'securerandom'
require 'audit'

class Assignment < ActiveRecord::Base
  def self.inheritance_column
    nil # TODO: For now; I might want to subclass this after all
  end
  enum assignment_kind: [:files, :questions, :exam]
  enum question_kind: [:yes_no, :true_false, :multiple_choice, :numeric, :text]

  belongs_to :blame, :class_name => "User", :foreign_key => "blame_id"

  belongs_to :course

  belongs_to :lateness_config

  belongs_to :related_assignment, :class_name => "Assignment", :foreign_key => "related_assignment_id"

  has_many :submissions, :dependent => :restrict_with_error
  has_many :used_subs, :dependent => :destroy

  has_many :assignment_graders, :dependent => :destroy
  has_many :graders, through: :assignment_graders

  validates :name,      :uniqueness => { :scope => :course_id }
  validates :name,      :presence => true
  validates :course_id, :presence => true
  validates :due_date,  :presence => true
  validates :available, :presence => true
  validates :blame_id,  :presence => true
  validates :points_available, :numericality => true
  validates :lateness_config, :presence => true
  validate  :valid_lateness_config

  def valid_lateness_config
    if !self.lateness_config.valid?
      self.lateness_config.errors.full_messages.each do |m|
        @errors[:base] << m
      end
    end
  end

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

  def sub_not_following_related?(user)
    # Is this submission not coming *after* any submissions to related assignments?
    related = Assignment.where(related_assignment_id: self.id)
    return related.all? do |a|
      a.submissions_for(user).empty?
    end
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

  # These two methods are needed to help download the entire assignment's submissions as a single tarball
  def tarball_path
    if tar_key.blank?
      self.tar_key = SecureRandom.hex(16)
      save!
    end

    dir = "downloads/#{tar_key}/"
    FileUtils.mkdir_p(Rails.root.join('public', dir))

    return '/' + dir + "assignment_#{id}.tgz"
  end

  def tarball_full_path
    Rails.root.join('public', tarball_path.sub(/^\//, ''))
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

  def questions
    if self.type == "questions"
      YAML.load(File.read(self.assignment_upload.submission_path))
    elsif self.type == "exam"
      qs = YAML.load(File.read(self.assignment_upload.submission_path))
      qs.each_with_index do |q, q_num|
        q["name"] = "Problem #{q_num + 1}" unless q["name"]
        if q["parts"]
          grade = 0
          q["parts"].each_with_index do |p, p_num|
            p["name"] = "Problem #{q_num + 1}#{('a'..'z').to_a[p_num]}" unless p["name"]
            grade += p["weight"]
          end
          q["weight"] = grade unless q["weight"]
        end
      end
      qs
    else
      nil
    end
  end

  def flattened_questions
    if self.type == "questions"
      qs = self.questions
      flat = []
      qs.each do |section|
        section.each do |name, qs|
          qs.each do |question|
            question.each do |type, q|
              q["type"] = type;
              flat.push q
            end
          end
        end
      end
      flat
    elsif self.type == "exam"
      qs = self.questions
      flat = []
      qs.each do |q|
        if q["parts"]
          q["parts"].each do |p|
            flat.push p
          end
        else
          flat.push q
        end
      end
      flat
    else
      nil
    end
  end
end
