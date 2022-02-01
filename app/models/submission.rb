# coding: utf-8
require 'securerandom'
require 'audit'
require 'addressable/uri'

class Submission < ApplicationRecord
  belongs_to :assignment
  belongs_to :user
  belongs_to :team
  belongs_to :upload
  belongs_to :comments_upload, class_name: "Upload"
  has_many :used_subs, dependent: :destroy
  has_many :grades
  has_many :reviews, class_name: "ReviewFeedback", dependent: :destroy
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

  def to_s
    "#{self.type} #{self.id} by #{if self.team then '(team ' + self.team_id.to_s + ') ' else '' end}" + self.users.map(&:name).to_sentence
  end

  def add_user_submissions!
    if team
      team.users.each do |u|
        UserSubmission.find_or_create_by!(user: u, submission: self)
      end
    elsif user
      UserSubmission.find_or_create_by!(user: user, submission: self)
    end
  end
  
  # Update the used submission for this submission/student OR submission/team
  # pair and update grader allocation if applicable.
  def set_used_everyone!
    # If team
    if team
      @same_team = self.same_team_as_old_used_sub
      # NOTE: If not all members of this team currently use the same submission,
      # leave existing grader allocations alone so they continue to be graded.
      team.users.each do |u|
        used = UsedSub.find_or_initialize_by(user_id: u.id, assignment_id: assignment.id)
        if @same_team && used.submission_id
          # TODO: Move grader allocation update to method?
          alloc = GraderAllocation.find_by(
            assignment_id: assignment_id,
            submission_id: used.submission_id)
          if alloc
            alloc.submission_id = self.id
            alloc.save
          end
        end
        used.submission = self
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
      used.submission = self
      used.save!
    end
  end

  # Update the used submission for this student/submission pair
  # and update grader allocation if applicable
  def set_used_student!(user_id)
    # TODO: In the team case, is there any situation in which there would
    # be a grader allocation to update?
    if team&.user_ids.includes(user_id) || (user&.id == user_id)
      used = UsedSub.find_or_initialize_by(user_id: user_id, assignment_id: assignment_id)
      if used.submission_id
        alloc = GraderAllocation.find_by(
          assignment_id: assignment_id,
          submission_id: used.submission_id)
        if alloc
          alloc.submission_id = self.id
          alloc.save
        end
      end
      used.submission = self
      used.save!
    else
      raise UserIDNotAssociated.new
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

  def hours_late(raw = false)
    self.assignment.sub_hours_late(self, raw)
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
    upload_id_will_change!
  end

  def save_upload(prof_override = nil)
    if @upload_data.nil?
      errors.add(:base, "You need to submit a file.")
      return false
    end

    data = @upload_data

    if data.size > course.sub_max_size.megabytes
      errors.add(:base, "Submitted file is too large (maximum size is #{course.sub_max_size}MB)")
      return false
    end

    up = Upload.new
    up.user_id = user.id
    up.assignment = assignment
    up.upload_data = data
    up.metadata = {
      type:       "Submission",
      user:       "#{user.name} (#{user.id})",
      course:     "#{course.name} (#{course.id})",
      assignment: "#{assignment.name} (#{assignment.id})",
      date:       Time.now.strftime("%Y/%b/%d %H:%M:%S %Z"),
      mimetype:   data.content_type,
      prof_override: prof_override
    }
    begin
      up.save!
      self.upload_id = up.id
      Audit.log("Sub #{self.id}: New submission upload by #{user.name} " +
                "(#{user.id}) with key #{up.secret_key}")
      return true
    rescue Exception => e
      errors.add(:base, e.message)
      return false
    end
  end

  def grade_line_comments(comment_author_user, show_hidden)
    config_types = Grade.where(submission_id: self.id).joins(:grader).pluck(:id, :type).to_h
    if show_hidden
      comments = self.inline_comments
    else
      comments = self.visible_inline_comments
    end
    comments.group_by(&:filename).map do |filename, byfile|
      [Upload.upload_path_for(filename), byfile.group_by(&:grade_id).map do |grade, bygrade|
         [grade, ([["type", config_types[grade]]] + bygrade.group_by(&:line).map do |line, byline|
            [line, byline.map{|c| c.to_editable_json(comment_author_user)}]
          end).to_h]
       end.to_h]
    end.to_h
    # self.gradrs.map(&:line_comments).reduce({}, &:merge)
  end

  def grade_submission_comments(show_hidden)
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
    up.assignment = assignment
    up.upload_data = data
    up.metadata = {
      type:       "Submission Comments",
      user:       "Some teacher for #{user.name} (#{user.id})",
      course:     "#{course.name} (#{course.id})",
      assignment: "#{assignment.name} (#{assignment.id})",
      date:       Time.now.strftime("%Y/%b/%d %H:%M:%S %Z"),
      mimetype:   data.content_type
    }
    up.save!

    self.comments_upload_id = up.id
    self.save!
  end

  def file_path
    if upload_id.nil?
      ""
    else
      Addressable::URI.encode_component(upload.path, Addressable::URI::CharacterClasses::PATH)
    end
  end

  def file_full_path
    if upload_id.nil?
      ""
    else
      upload.submission_path
    end
  end

  def recreate_missing_grade(config)
    if config.grade_exists_for(self)
      false
    else
      if config.autograde?
        # Students will have their delayed jobs de-prioritized based on how many submissions
        # they've spammed the system with in the past fifteen minutes
        num_recent_attempts = assignment.submissions_for(user).where("created_at > ?", Time.now - 15.minutes).count
        config.autograde!(assignment, self, num_recent_attempts)
        true
      else
        config.ensure_grade_exists_for!(self)
      end
    end
  end

  def recreate_missing_grades(configs)
    configs.reduce(0) do |sum, c|
      if recreate_missing_grade(c)
        sum + 1
      else
        sum
      end
    end
  end

  def create_grades!
    assignment.graders.each do |gr| gr.ensure_grade_exists_for!(self) end
  end

  def autograde!(delay = false)
    complete = true
    num_recent_attempts =
      if delay
        # Students will have their delayed jobs de-prioritized based on how many submissions
        # they've spammed the system with in the past fifteen minutes
        assignment.submissions_for(user).where("created_at > ?", Time.now - 15.minutes).count
      else
        0
      end
    assignment.graders.each do |gr|
      complete = complete && gr.autograde?
      begin
        gr.autograde!(assignment, self, num_recent_attempts) # make sure we create all needed grades
      rescue Exception => e
        Audit.log "Assignment #{assignment.id}, submission #{self.id} failed autograding:"
        Audit.log e.inspect
        Audit.log e.backtrace
        complete = false
      end
    end
    self.compute_grade! if complete
  end

  def plagiarism_status
    InlineComment.where(submission: self, label: "Plagiarism")
  end
  def self.plagiarism_status(subs)
    InlineComment.where(submission: subs, label: "Plagiarism").group_by(&:submission_id)
  end
  
  def compute_grade!
    ### A Submission's score is recorded as a percentage
    score = 0.0
    max_score = 0.0
    max_score_with_ec = 0.0
    log = ""
    self.grades.includes(:grader).each do |g|
      return if g.score.nil?
      component_weight = g.grader.avail_score.to_f # Ignore normal/e.c. distinction for now
      if (g.out_of.to_f == 0.0)
        grade_component = 0.0
      else
        grade_component = component_weight * (g.score.to_f / g.out_of.to_f)
      end
      log += "#{g.grader.type}#{if g.grader.extra_credit then ' (E.C.)' end} => #{grade_component} / #{component_weight}, "
      score += grade_component
      max_score += g.grader.normal_weight # IGNORING any extra credit weight
      # NOTE: normal_weight + extra_credit_weight =?= component_weight
      # In manual graders, this will definitely be an equality, because they are mutually exclusive
      # But in exam graders, this may not be an equality, because exams can have e.c. questions
      # separately from their normal questions.
      max_score_with_ec += g.grader.normal_weight + g.grader.extra_credit_weight
    end
    plagiarism = self.plagiarism_status
    if plagiarism.count > 0
      penalty = plagiarism.pluck(:weight).sum
      log += "Plagiarism penalty => #{penalty}"
      score -= penalty
    end
    if (max_score.to_f == 0)
      self.score = 0
    else
      self.score = (100.0 * score.to_f) / max_score.to_f
      if self.ignore_late_penalty
        log += "Ignoring late penalty, "
      else
        max_pct = 100.0 * (max_score_with_ec.to_f / max_score.to_f)
        self.score = assignment.lateness_config.penalize(self.score, assignment, self, 100.0, max_pct)
      end
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
    grades.count == assignment.graders.count &&
    grades.all?(&:available)
  end

  def get_submission_files(current_user, line_comments = nil, show_deductions = false)
    show_hidden = (current_user&.site_admin? || current_user&.course_staff?(@course))
    @lineCommentsByFile = line_comments || self.grade_line_comments(nil, show_hidden)
    @submission_files = []
    @show_deductions = show_deductions
    def ensure_utf8(str, mimetype)
      if ApplicationHelper.binary?(mimetype)
        str
      else
        if str.is_utf8?
          str
        else
          begin
            String.new(str).encode(Encoding::UTF_8, invalid: :replace, undef: :replace, replace: '?')
          rescue Exception => e
            str
          end
        end
      end
    end
    def with_extracted(item)
      return nil if item.nil?
      if item[:public_link]
        return nil if File.basename(item[:full_path].to_s) == ".DS_Store"
        comments = @lineCommentsByFile[item[:public_link].to_s] || {noCommentsFor: item[:public_link].to_s}
        mimetype = ApplicationHelper.mime_type(item[:full_path])
        contents = begin
                     File.read(item[:full_path].to_s)
                   rescue Errno::EACCES => e
                     "Could not access file:\n#{e.to_s.gsub(item[:full_path].to_s, item[:public_link].to_s)}"
                   rescue Errno::ENOENT => e
                     "Somehow, #{item[:public_link]} does not exist"
                   rescue Exception => e
                     "Error reading file:\n#{e.to_s.gsub(item[:full_path].to_s, item[:public_link].to_s)}"
                   end
        @submission_files.push({
          link: item[:public_link],
          name: item[:public_link].sub(/^.*extracted\//, ""),
          pdf_path: item[:converted_path],
          contents: ensure_utf8(contents, mimetype),
          type: mimetype,
          href: @submission_files.count + 1,
          lineComments: comments
          })
        deductions =
          if comments[:noCommentsFor]
            nil
          elsif @show_deductions
            comments.reduce(nil) do |sum, (gradeId, commentsByGradeId)|
              if commentsByGradeId.is_a? String
                sum
              elsif (@show_deductions.is_a? Integer) && (@show_deductions != gradeId)
                sum
              else
                commentsByGradeId.reduce(sum) do |sum, (line, comments)|
                  if line == "type"
                    sum
                  else
                    comments.reduce(sum) do |sum, comment|
                      if comment[:severity] == "Bonus"
                        sum.to_f + comment[:deduction]
                      else
                        sum.to_f - comment[:deduction]
                      end
                    end
                  end
                end
              end
            end
          end
        { text:
            if deductions.to_f > 0
              "#{item[:path]} (+#{deductions})"
            elsif deductions
              "#{item[:path]} (#{deductions})"
            else
              item[:path]
            end,
          href: @submission_files.count,
          #icon: @lineCommentsByFile[item[:public_link].to_s] ? "glyphicon glyphicon-flash" : ""
        }
      elsif item[:link_to]
        @submission_files.push({
          link_to: item[:link_to].sub(/^.*extracted\//, ""),
          name: item[:path],
          type: "symlink",
          href: @submission_files.count + 1,
          lineComments: {noCommentsFor: item[:path].to_s},
          broken: item[:broken]
          })
        {
          text: item[:path] + " " + (item[:broken] ? "↯" : "⤏"),
          href: @submission_files.count
        }
      else
        return nil if item[:path] == "__MACOSX"
        {
          text: item[:path] + "/",
          state: {selectable: true},
          nodes: item[:children].map{|i| with_extracted(i)}.compact
        }
      end
    end

    @submission_dirs = self.upload.extracted_files.map{|i| with_extracted(i)}.compact
    @submission_files.each do |sf|
      if sf[:type] == "symlink" && !sf[:broken]
        sf[:link_href] = @submission_files.find{|f| f[:link]&.ends_with?(sf[:link_to])}[:href]
      end
    end

    @count = @submission_files.count.to_s.length

    def fix_hrefs(node)
      if node[:href].is_a? Integer
        node[:href] = "#file_" + node[:href].to_s.rjust(@count, '0')
      end
      if node[:link_href].is_a? Integer
        node[:link_href] = "#file_" + node[:link_href].to_s.rjust(@count, '0')
      end
      if node[:nodes]
        node[:nodes].each do |n| fix_hrefs(n) end
      end
    end
    sub_dirs = fix_hrefs({nodes: @submission_dirs})
    sub_files = fix_hrefs({nodes: @submission_files})
    remove_instance_variable :@show_deductions
    remove_instance_variable :@lineCommentsByFile
    remove_instance_variable :@submission_dirs
    remove_instance_variable :@submission_files
    return sub_dirs, sub_files
  end



  protected

  def user_is_registered_for_course
    return unless self.new_record? || self.user_id_changed?
    if user && !user.courses.any?{|cc| cc.id == course.id }
      errors.add(:base, "Not registered for this course :" + user.name)
    end
    if team && !team.users.all?{|u| u.courses.any?{|cc| cc.id == course.id } }
      errors.add(:base, "Not registered for this course :" + team.users.map{|u| u.name}.to_s)
    end
  end

  def submitted_file_or_manual_grade
    return unless self.new_record? || self.upload_id_changed?
    if upload_id.nil? && @upload_data.nil? && self.assignment.type != "Exam"
      errors.add(:base, "You need to submit a file.")
    end
  end

  def file_below_max_size
    return unless self.new_record? || self.upload_size_changed?
    msz = course.sub_max_size
    if upload_size > msz.megabytes
      errors.add(:base, "Upload exceeds max size (#{upload_size} > #{msz} MB).")
    end
  end

  def has_team_or_user
    return unless self.new_record? || self.team_id_changed? || self.user_id_changed?
    if assignment.team_subs?
      if team.nil?
        errors.add(:base, "Assignment requires team subs. No team set.")
      end
    else
      if user.nil?
        errors.add(:base, "Assignment requires individual subs. No user set.")
      elsif team
        errors.add(:base, "Assignment requires individual subs. Team should not be set.")
      end
    end
  end

  private

  def same_team_as_old_used_sub
    used_subs_for_team_users = UsedSub.where(assignment: assignment, user: team.users).includes(:submission)
    return used_subs_for_team_users.all? { |u| u.submission.team_id == team.id } 
  end

  # TODO: Should this be a method in team, or
  # is there potentially a way to do this without?
  def user_id_in_sub_team(user_id)
    team.users.each do |u|
      if u.id == user_id
        return true
      end
    end
    return false
  end


end

class UserIDNotAssociated < StandardError
  def initialize(msg="The given user ID is not associated with this submission.")
    super(msg)
  end
end
