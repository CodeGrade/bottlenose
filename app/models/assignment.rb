require 'securerandom'
require 'audit'
require 'addressable/uri'

class Assignment < ApplicationRecord
  enum question_kind: [:yes_no, :true_false, :multiple_choice, :numeric, :text]

  attr_accessor :removefile
  attr_accessor :teamset_plan
  attr_accessor :teamset_source_use
  attr_accessor :teamset_source_copy
  attr_accessor :current_user

  before_create :stifle_graders
  after_create :restore_graders
  after_save :save_upload
  
  before_validation :establish_teamsets

  belongs_to :blame, :class_name => "User", :foreign_key => "blame_id"

  belongs_to :course

  belongs_to :teamset

  belongs_to :lateness_config
  accepts_nested_attributes_for :lateness_config

  belongs_to :related_assignment, :class_name => "Assignment", :foreign_key => "related_assignment_id"

  has_many :submissions, :dependent => :restrict_with_error
  has_many :used_subs, :dependent => :destroy

  has_many :graders, dependent: :destroy, autosave: true
  accepts_nested_attributes_for :graders, allow_destroy: true

  has_many :grader_allocations, dependent: :destroy
  
  has_many :interlocks, dependent: :destroy
  has_many :related_interlocks, :foreign_key => "related_assignment_id", :class_name => "Interlock"
  accepts_nested_attributes_for :interlocks, allow_destroy: true
  has_many :submission_views
  has_many :submission_enabled_toggles

  has_many :codereview_matchings, dependent: :destroy
  has_many :individual_extensions, dependent: :destroy
  
  validates :name,      :uniqueness => { :scope => :course_id }
  validates :name,      :presence => true
  validates :course_id, :presence => true
  validates :due_date,  :presence => true
  validates :available, :presence => true
  validates :blame_id,  :presence => true
  validates :points_available, :numericality => true
  validates :lateness_config, :presence => true
  validates :graders, :presence => true


  def submission_prohibited(submission, staff_override)
    return false if staff_override

    return allowed_for_lateness(submission) ||
           sub_after_related(submission.user) ||
           sub_needs_team(submission) ||
           rate_limit(submission) ||
           submissions_interlocked(submission.user, submission.team) ||
           false # no other reasons to reject the submission
  end

  def legal_teamset_actions
    # Returns the set of actions on teamsets that are legal as of the saved values in the database
    # Any unsaved changes do not impact this decision, so that validations can check that any action
    # taken is plausible.
    # All actions are present, either as a dictionary of some specializing properties,
    # or as an explanatory error message of why the action isn't currently legal
    if self.new_record?
      {
        no_teams: {checked: true},
        new: {existing: false},
        use: {existing: false},
        copy: {existing: false},
        clone: "New assignment doesn't have a teamset yet to clone",
        unique: "New assignment doesn't yet have a unique teamset"
      }
    else
      legal_actions = {}
      existing_subs = !submissions.empty?
      orig_team_subs = self.changes[:team_subs]&.at(0) || self.team_subs?
      orig_teamset_id = self.changes[:teamset_id]&.at(0) || self.teamset_id
      if !existing_subs # Currently no submissions ==> anything goes
        legal_actions[:no_teams] = {checked: !orig_team_subs}
        legal_actions[:new] = {existing: false}
        legal_actions[:use] = {existing: false, checked: orig_team_subs, value: orig_teamset_id}
        legal_actions[:copy] = {existing: false}
        legal_actions[:clone] = "No need to clone this teamset, since nothing has been submitted through it yet"
        legal_actions[:unique] = "No need to restrict this teamset yet, since nothing has been submitted through it"
      elsif !orig_team_subs # currently solo, currently some submissions ==> can't `use`
        legal_actions[:no_teams] = {checked: true}
        legal_actions[:new] = {existing: true}
        legal_actions[:copy] = {existing: true}
        legal_actions[:use] = "Cannot reuse an existing teamset since there are already solo submissions in this one"
        legal_actions[:clone] = "This assignment already has solo submissions in it"
        legal_actions[:unique] = "This assignment isn't using teams; it doesn't need to choose a teamset"
      elsif self.teamset.assignments.count == 1 # this is the only user of this teamset ==> nothing to do
        legal_actions[:unique] = true
        legal_actions[:no_teams] = "This assignment already has team submissions in it"
        legal_actions[:new] = "This assignment already has a unique teamset"
        legal_actions[:copy] = "This assignment already has a unique teamset"
        legal_actions[:use] = "Cannot reuse an existing teamset since there are already submissions in this one"
        legal_actions[:clone] = "This assignment already has a unique teamset"
      else # shared teamset, team submissions, existing submissions ==> can't use or copy anything but this one
        legal_actions[:use] = {existing: true, checked: true, value: orig_teamset_id}
        legal_actions[:clone] = {existing: true}
        legal_actions[:unique] = "This assignment shares its teamset with others; it can't be unique"
        legal_actions[:no_teams] = "This assignment already has team submissions in it"
        legal_actions[:new] = "This assignment already has a teamset with submissions in it"
        legal_actions[:copy] = "This assignment already has a teamset with submissions in it"
      end
      legal_actions
    end
  end

  def establish_teamsets
    if self.new_record?
      setup_teamsets
    else
      update_teamsets
    end
  end
  def setup_teamsets
    # Options are:
    # None -- create a (unique, ignored) teamset
    # New -- create a new teamset with initially no teams
    # Use -- share an existing teamset with another assignment
    # Copy -- duplicate an existing teamset
    
    return true if (!self.teamset.nil?) && @teamset_plan.nil?
    
    action = legal_teamset_actions[@teamset_plan.to_sym]
    if action.is_a? String
      self.errors.add(:base, "Impossible state: cannot #{@teamset_plan} teamset because: #{action}")
      return false
    end
    self.team_subs = (@teamset_plan != "none")
    if @teamset_plan == "new" || @teamset_plan == "none"
      self.teamset = Teamset.create(course: self.course, name: "Team set for #{self.name}")
    elsif @teamset_plan == "copy"
      if @teamset_source_copy.empty?
        self.errors.add(:base, "The teamset to be copied was not specified")
        return false
      else
        ts = Teamset.find(@teamset_source_copy.to_i)
        if ts.nil?
          self.errors.add(:base, "The specified teamset to be copied does not exist")
          return false
        end
        self.teamset = ts.dup(nil, "Teamset for #{self.name}")
      end
    elsif @teamset_plan == "use"
      if @teamset_source_use.empty?
        self.errors.add(:base, "The reference teamset was not specified")
        return false
      else
        ts = Teamset.find(@teamset_source_use.to_i)
        self.teamset = ts
      end
    end
    return true
  end

  def update_teamsets
    # Options are:
    # None -- leave the (unique, ignored) teamset alone
    # New -- create solo teams for everyone who's already submitted
    # Copy -- copy an existing teamset, and create solo teams for anyone who's already submitted
    # Unique -- nothing to be done; it's already a unique teamset
    # Use -- nothing to be done; continue to share teamset with another assignment
    # Clone -- duplicate the teamset and transfer over any submissions
    
    return true if (!self.teamset.nil?) && @teamset_plan.nil?

    action = legal_teamset_actions[@teamset_plan.to_sym]
    if action.is_a? String
      self.errors.add(:base, "Impossible state: cannot #{@teamset_plan} teamset because: #{action}")
      return false
    end
    if @teamset_plan == "none"
      if self.team_subs #still the original value
        # was a teamset; now changing to be individual subs
        self.teamset = Teamset.create(course: self.course, name: "Team set for #{self.name}")
        self.teamset.make_solo_teams_for(self)
      else
        # nothing to do        
      end
    elsif @teamset_plan == "new"
      self.teamset.make_solo_teams_for(self)
    elsif @teamset_plan == "copy"
      if @teamset_source_copy.blank?
        self.errors.add(:base, "The teamset to be copied was not specified")
        return false
      end
      ts = Teamset.find(@teamset_source_copy.to_i)
      if ts.nil?
        self.errors.add(:base, "The specified teamset to be copied does not exist")
        return false
      end
      self.teamset = ts.dup(nil, "Teamset for #{self.name}")
      self.teamset.make_solo_teams_for(self)
    elsif @teamset_plan == "unique"
      # nothing to do
    elsif @teamset_plan == "use"
      if action[:existing]
        # nothing to do
      elsif @teamset_source_use.blank?
        self.errors.add(:base, "The teamset to be used was not specified")
        return false
      else
        ts = Teamset.find(@teamset_source_use.to_i)
        self.teamset = ts
      end
    elsif @teamset_plan == "clone"
      if self.teamset.assignments.count > 1 # no need to dup if it's already unique
        self.teamset = self.teamset.dup(self, "Teamset for #{self.name}")
      end
    end
    self.team_subs = (@teamset_plan != "none")
  end

  def effective_sub_due_date(sub)
    if @cached
      return @cached[:subs][sub.user_id] || self.due_date
    elsif self.team_subs
      self.individual_extensions.find_by(team_id: sub.team_id)&.due_date || self.due_date
    else
      self.individual_extensions.find_by(user_id: sub.user_id)&.due_date || self.due_date
    end
  end
  
  def effective_due_date(user, team)
    if @cached
      return @cached[:all][user.id] || self.due_date
    elsif self.team_subs
      self.individual_extensions.find_by(team: team)&.due_date || self.due_date
    else
      self.individual_extensions.find_by(user: user)&.due_date || self.due_date
    end
  end

  def extensions_for_users(users, only_used_subs, as_of = DateTime.now)
    if users.is_a? User
      users = [users]
    end
    if @cached
      if only_used_subs
        return users.map{|u| [u.id, @cached[:all][u.id] || self.due_date]}.to_h
      else
        return users.map{|u| [u.id, @cached[:subs][u.id] || self.due_date]}.to_h
      end
    elsif self.team_subs
      if only_used_subs
        used_subs = multi_group_by(self.all_used_subs.where(user: users).includes(:team, :user_submissions).references(:team, :user_submissions), [:user_id], true)
        teams = used_subs.map{|_, s| s.user_submissions.map{|us| [us.user_id, s.team]}}.flatten(1).to_h
      else
        teams = self.teamset.active_teams_for(users, as_of)
      end
      extensions = self.individual_extensions.where(team: teams.values)
      extensions = multi_group_by(extensions, [:team_id], true)
      users.map do |u|
        [u.id, extensions[teams[u.id]&.id]&.due_date]
      end.to_h
    else
      extensions = multi_group_by(self.individual_extensions.where(user: users), [:user_id], true)
      users.map{|u| [u.id, extensions[u.id]&.due_date]}.to_h
    end
  end

  def effective_due_dates(users, only_used_subs)
    extensions_for_users(users, only_used_subs, self.due_date).map do |uid, ext|
      [uid, ext || self.due_date]
    end.to_h
  end

  def cache_effective_due_dates!(users)
    @cached = nil
    @cached = {
      subs: effective_due_dates(users, true),
      all: effective_due_dates(users, false)
    }
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

  def destroy
    au = self.assignment_upload
    au.destroy unless au.nil?
    self.assignment_upload_id = nil;
    super
  end
  
  def assignment_upload
    Upload.find_by(id: assignment_upload_id)
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

  def assignment_file_path
    if assignment_upload_id.nil?
      ""
    else
      Addressable::URI.encode_component(assignment_upload.path, Addressable::URI::CharacterClasses::PATH)
    end
  end
  def assignment_file=(data)
    @assignment_file_data = data
  end

  def stifle_graders
    @_graders = self.graders.to_a
    self.graders = []
  end

  def restore_graders
    @_graders.each do |g|
      g.assignment = self
      g.assignment_id = self.id
      g.save!
    end
    self.graders.reload
    return true
  end

  def save_upload
    return true if @inSave
    user = User.find(blame_id)

    if @assignment_file_data.nil?
      return "Nothing to save"
    else
      unless assignment_upload_id.nil?
        Audit.log("Assn #{id}: Orphaning assignment upload " +
                  "#{assignment_upload_id} (#{assignment_upload.secret_key})")
      end

      up = Upload.new
      up.user_id = user.id
      up.assignment = self
      begin
        up.upload_data = @assignment_file_data
        up.metadata = {
          type:       "Assignment File",
          user:       "#{user.name} (#{user.id})",
          course:     "#{course.name} (#{course.id})",
          date:       Time.now.strftime("%Y/%b/%d %H:%M:%S %Z"),
          mimetype:   @assignment_file_data.content_type
        }
        up.save!
      rescue Exception => e
        errors.add(:base, e.message)
        raise ActiveRecord::RecordInvalid.new(self)
      end

      # This is pretty gunky.  We need the assignment id in Upload in order to know
      # what directory to put the files in.  But we don't have the id until *after*
      # initially saving ourself.  Of course, once we do that, we need to save
      # ourselves again, with the updated assignment_upload field...but that would be
      # infinitely recursive.  So prevent the regress by disabling this after_save callback
      self.assignment_upload_id = up.id
      @assignment_file_data = nil
      oldSave = @inSave
      @inSave = true
      self.save
      @inSave = oldSave
      
      Audit.log("Assn #{id}: New assignment file upload by #{user.name} " +
                "(#{user.id}) with key #{up.secret_key}")
      return true
    end
  end

  # These two methods are needed to help download the entire assignment's submissions as a single tarball
  def tarball_path
    if tar_key.blank?
      self.tar_key = SecureRandom.hex(16)
      save!
    end

    dir = "downloads/#{tar_key}/"
    FileUtils.mkdir_p(Rails.root.join('private', dir))

    return '/' + dir + "assignment_#{id}.tgz"
  end

  def tarball_full_path
    Rails.root.join('private', tarball_path.sub(/^\//, ''))
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
    UsedSub.find_by(user_id: user.id, assignment_id: self.id)&.submission
  end

  def self.cached_grades_complete(assns, subs)
    completed_grades = Grade.where(submission: subs, available: true).group(:submission_id).count
    graders = Grader.where(assignment: assns).group(:assignment_id).count
    subs.map do |s|
      [s.id, completed_grades[s.id] == graders[s.assignment_id]]
    end.to_h
  end
  
  def main_submissions
    used_subs.includes(:subsmissions).map do |sfg|
      sfg.submission
    end
  end

  def graders_ordered
    graders.order(:order)
  end

  def lateness_config_attributes=(attrs)
    self.lateness_config = LatenessConfig.find_or_initialize_by(id: attrs[:id])
    self.lateness_config.assign_attributes(attrs)
  end

  def assign_attributes(attrs)
    if attrs[:removefile] == "remove"
      attrs[:assignment_file] = nil
      self.assignment_upload_id = nil
    end
    attrs.delete :removefile
    attrs[:graders_attributes]&.each do |k, v|
      v[:assignment] = self
    end
    super(attrs)
  end


  private
  
  def allowed_for_lateness(sub)
    if !self.lateness_config.allow_submission?(self, sub)
      "It's too late to submit to this assignment: " +
        "you either have too few late days remaining, " +
        "or it's too long after the assignment is due."
    else
      false
    end
  end

  def sub_after_related(user)
    # Is this submission not coming *after* any submissions to related assignments?
    related = Assignment.where(related_assignment_id: self.id)
    if !related.all?{|a| a.submissions_for(user).empty?}
      "You are not allowed to resubmit your assignment after you've already submitted to a related assignment"
    else
      false
    end
  end

  def sub_needs_team(sub)
    if sub.team.nil? && self.team_subs?
      "This assignment requires team #{self.type == 'Files' ? 'submissions' : 'responses'}," +
        " and you are not currently in a team.  Contact a professor to join a team."
    else
      false
    end
  end

  def rate_limit(sub)
    if (self.max_attempts.to_i > 0) && (self.submissions.count >= self.max_attempts.to_i)
      "You cannot attempt more #{self.type == 'Files' ? 'submissions' : 'responses'} for this assignment: " +
        "you've reached the maximum number of attempts allowed."
    elsif (self.rate_per_hour.to_i > 0) &&
          (self.submission.where('created_at >= ?', DateTime.now - 1.hour).count > self.rate_per_hour.to_i)
      "You cannot #{self.type == 'Files' ? 'submit' : 'respond'} to this assignment right now: " +
        "you've tried too many times in the past hour.  Please wait a while before trying again."
    else
      false
    end
  end

  def submissions_interlocked(user, team)
    locks = self.interlocks.group_by(&:constraint)
    locks["no_submission_unless_submitted"]&.each do |lock|
      if lock.related_assignment.submissions_for(user).empty?
        return "You have not submitted to #{lock.related_assignment.name}, and so cannot submit to this assignment"
      end
    end
    locks["no_submission_after_viewing"]&.each do |lock|
      views =
        if self.team_subs?
          SubmissionView.where(assignment: lock.related_assignment, team: team)
        else
          SubmissionView.where(assignment: lock.related_assignment, user: user)
        end
      if !views.empty?
        return "You (or a teammate) have already viewed #{lock.related_assignment.name}, and so cannot submit to this assignment"
      end
    end
    return false
  end


end
