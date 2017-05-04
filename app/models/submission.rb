require 'securerandom'
require 'audit'

class Submission < ActiveRecord::Base
  def self.inheritance_column
    nil
  end
  belongs_to :assignment
  belongs_to :user
  belongs_to :team
  belongs_to :upload
  belongs_to :comments_upload, class_name: "Upload"
  has_many :used_subs, dependent: :destroy
  has_many :grades
  has_many :user_submissions, dependent: :destroy
  has_many :users, through: :user_submissions
  has_many :inline_comments, dependent: :destroy

  validates :assignment_id, :presence => true

  validate :has_team_or_user
  validate :user_is_registered_for_course
  validate :submitted_file_or_manual_grade
  validate :file_below_max_size

  delegate :course,    :to => :assignment
  delegate :file_name, :to => :upload, :allow_nil => true

  before_destroy :cleanup!
  after_save :add_user_submissions!

  def add_user_submissions!
    if team
      team.users.each do |u|
        UserSubmission.find_or_create_by!(user: u, submission: self)
      end
    elsif user
      UserSubmission.find_or_create_by!(user: user, submission: self)
    end
  end

  def set_used_sub!
    if team
      @same_team = true
      team.users.each do |u|
        used = UsedSub.find_by(user_id: u.id, assignment_id: assignment.id)
        if used
          old_team = used.submission.team
          if old_team.nil? || (old_team.id != team.id)
            @same_team = false
          end
        end
      end
      team.users.each do |u|
        used = UsedSub.find_or_initialize_by(user_id: u.id, assignment_id: assignment.id)
        if @same_team and used.submission_id
          alloc = GraderAllocation.find_by(
            assignment_id: assignment_id,
            submission_id: used.submission_id)
          if alloc
            alloc.submission_id = self.id
            alloc.save
          end
        end
        used.submission_id = self.id
        used.save!
      end
    else
      used = UsedSub.find_or_initialize_by(user_id: user.id, assignment_id: assignment_id)
      if used.submission_id
        alloc = GraderAllocation.find_by(
          assignment_id: assignment_id,
          submission_id: used.submission_id)
        if alloc
          alloc.submission_id = self.id
          alloc.save
        end
      end
      used.submission_id = self.id
      used.save!
    end
  end

  def submission_users
    if team
      team.users
    else
      [user]
    end
  end

  def submission_user_names
    if team
      team.member_names
    else
      user.name
    end
  end

  def name
    "#{user.name} @ #{created_at}"
  end

  def late?
    self.assignment.sub_late?(self)
  end

  def days_late(raw = false)
    self.assignment.sub_days_late(self, raw)
  end

  def late_penalty
    self.assignment.sub_late_penalty(self)
  end

  def upload_file=(data)
    return if data.nil?

    unless upload_id.nil?
      raise Exception.new("Attempt to replace submission upload.")
    end

    self.upload_size = data.size
    @upload_data = data
  end

  def save_upload
    if @upload_data.nil?
      errors[:base] << "You need to submit a file."
      return false
    end

    data = @upload_data

    if data.size > course.sub_max_size.megabytes
      return false
    end

    up = Upload.new
    up.user_id = user.id
    begin
      up.store_upload!(data, {
        type:       "Submission",
        user:       "#{user.name} (#{user.id})",
        course:     "#{course.name} (#{course.id})",
        assignment: "#{assignment.name} (#{assignment.id})",
        date:       Time.now.strftime("%Y/%b/%d %H:%M:%S %Z"),
        mimetype:   data.content_type
      })
    rescue Exception => e
      errors[:base] << e.message
      return false
    end
    if up.save
      self.upload_id = up.id

      Audit.log("Sub #{id}: New submission upload by #{user.name} " +
                "(#{user.id}) with key #{up.secret_key}")
      return true
    else
      return false
    end
  end

  def grader_line_comments(comment_author_user, show_hidden)
    config_types = Grader.where(submission_id: self.id).joins(:grader_config).pluck(:id, :type).to_h
    if show_hidden
      comments = self.inline_comments
    else
      comments = self.visible_inline_comments
    end
    comments.group_by(&:filename).map do |filename, byfile|
      [Upload.upload_path_for(filename), byfile.group_by(&:grader_id).map do |grader, bygrader|
         [config_types[grader], bygrader.group_by(&:line).map do |line, byline|
            [line, byline.map{|c| c.to_editable_json(comment_author_user)}]
          end.to_h]
       end.to_h]
    end.to_h
    # self.graders.map(&:line_comments).reduce({}, &:merge)
  end

  def grader_submission_comments(show_hidden)
    if show_hidden
      self.inline_comments.where(line: 0).group_by(&:filename)
    else
      self.visible_inline_comments.where(line: 0).group_by(&:filename)
    end
  end

  def visible_inline_comments
    self.inline_comments.joins(:grade).where("grades.available": true)
  end

  def comments_upload_file=(data)
    return if data.nil?

    unless comments_upload_id.nil?
      comments_upload.destroy
    end

    up = Upload.new
    up.user_id = user.id
    up.store_upload!(data, {
      type:       "Submission Comments",
      user:       "Some teacher for #{user.name} (#{user.id})",
      course:     "#{course.name} (#{course.id})",
      assignment: "#{assignment.name} (#{assignment.id})",
      date:       Time.now.strftime("%Y/%b/%d %H:%M:%S %Z"),
      mimetype:   data.content_type
    })
    up.save!

    self.comments_upload_id = up.id
    self.save!
  end

  def file_path
    if upload_id.nil?
      ""
    else
      upload.path
    end
  end

  def file_full_path
    if upload_id.nil?
      ""
    else
      upload.submission_path
    end
  end

  def recreate_missing_grader(config)
    if config.grader_exists_for(self)
      false
    else
      config.delay.autograde!(assignment, self)
      true
    end
  end

  def recreate_missing_graders(configs)
    configs.reduce(0) do |sum, c|
      if recreate_missing_grader(c)
        sum + 1
      else
        sum
      end
    end
  end

  def create_graders!
    assignment.graders.each do |c| c.ensure_grade_exists_for!(self) end
  end

  def autograde!
    complete = true
    assignment.graders.each do |c|
      begin
        complete = complete and c.autograde?
        c.autograde!(assignment, self) # make sure we create all needed graders
      rescue Exception => e
        puts e.inspect
        puts e.backtrace
        complete = false
      end
    end
    self.compute_grade! if complete
  end

  def compute_grade!
    ### A Submission's score is recorded as a percentage
    score = 0.0
    max_score = 0.0
    log = ""
    self.grades.each do |g|
      return if g.score.nil?
      component_weight = g.grader_config.avail_score.to_f
      grade_component = component_weight * (g.score.to_f / g.out_of.to_f)
      log += "#{g.grader_config.type} => #{grade_component} / #{component_weight}, "
      score += grade_component
      max_score += component_weight
    end
    plagiarism = InlineComment.where(submission: self, label: "Plagiarism")
    if plagiarism.count > 0
      penalty = plagiarism.pluck(:weight).sum
      log += "Plagiarism penalty => #{penalty}"
      score -= penalty
    end
    self.score = (100.0 * score.to_f) / max_score.to_f
    if self.ignore_late_penalty
      log += "Ignoring late penalty, "
    else
      self.score = assignment.lateness_config.penalize(self.score, assignment, self)
    end
    log += "Final score: #{self.score}%"

    Audit.log("Grading submission at #{DateTime.current}, grades are #{log}")
    self.save!

    # root = Rails.root.to_s
    # system(%Q{(cd "#{root}" && script/grade-submission #{self.id})&})
  end

  def cleanup!
    upload.cleanup! unless upload.nil?
  end

  def visible_to?(user)
    user.course_staff?(course) ||
      UserSubmission.exists?(user: user, submission: self)
  end

  def grade_complete?
    graders.count == assignment.grader_configs.count &&
    graders.all?(&:available)
  end

  private

  def user_is_registered_for_course
    if user && !user.courses.any?{|cc| cc.id == course.id }
      errors[:base] << "Not registered for this course :" + user.name
    end
    if team && !team.users.all?{|u| u.courses.any?{|cc| cc.id == course.id } }
      errors[:base] << "Not registered for this course :" + team.users.map{|u| u.name}.to_s
    end
  end

  def submitted_file_or_manual_grade
    if upload_id.nil? and self.assignment.type != "exam"
      errors[:base] << "You need to submit a file."
    end
  end

  def file_below_max_size
    msz = course.sub_max_size
    if upload_size > msz.megabytes
      errors[:base] << "Upload exceeds max size (#{upload_size} > #{msz} MB)."
    end
  end

  def has_team_or_user
    if assignment.team_subs?
      if team.nil?
        errors[:base] << "Assignment requires team subs. No team set."
      end
    else
      if user.nil?
        errors[:base] << "Assignment requires individual subs. No user set."
      elsif team
        errors[:base] << "Assignment requires individual subs. Team should not be set."
      end
    end
  end
end
