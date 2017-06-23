require 'securerandom'
require 'audit'

class Assignment < ApplicationRecord
  def self.inheritance_column
    nil # TODO: For now; I might want to subclass this after all
  end
  enum assignment_kind: [:files, :questions, :exam]
  enum question_kind: [:yes_no, :true_false, :multiple_choice, :numeric, :text]

  attr_accessor :removefile
  attr_accessor :teamset_plan
  attr_accessor :teamset_source_use
  attr_accessor :teamset_source_copy

  belongs_to :blame, :class_name => "User", :foreign_key => "blame_id"

  belongs_to :course

  belongs_to :teamset

  belongs_to :lateness_config
  accepts_nested_attributes_for :lateness_config

  belongs_to :related_assignment, :class_name => "Assignment", :foreign_key => "related_assignment_id"

  has_many :submissions, :dependent => :restrict_with_error
  has_many :used_subs, :dependent => :destroy

  has_many :assignment_graders, :dependent => :destroy
  has_many :graders, through: :assignment_graders
  accepts_nested_attributes_for :graders, allow_destroy: true

  validates :name,      :uniqueness => { :scope => :course_id }
  validates :name,      :presence => true
  validates :course_id, :presence => true
  validates :due_date,  :presence => true
  validates :available, :presence => true
  validates :blame_id,  :presence => true
  validates :points_available, :numericality => true
  validates :lateness_config, :presence => true
  before_create :fixup_graders

  before_create :setup_teamsets
  before_update :update_teamsets
  before_update :update_exam_submission_times

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
        self.teamset = ts.dup
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
      # nothing to do
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
      self.teamset = ts.dup
      self.teamset.make_solo_teams_for(self)
    elsif @teamset_plan == "unique"
      # nothing to do
    elsif @teamset_plan == "use"
      if @teamset_source_use.blank?
        self.errors.add(:base, "The teamset to be used was not specified")
        return false
      else
        ts = Teamset.find(@teamset_source_use.to_i)
        self.teamset = ts
      end
    elsif @teamset_plan == "clone"
      if self.teamset.assignments.count > 1 # no need to dup if it's already unique
        self.teamset = self.teamset.dup(self)
      end
    end
    self.team_subs = (@teamset_plan != "none")
  end

  def update_exam_submission_times
    return if self.type != "exam"
    self.available = self.due_date # for exams, there's no window in which "the assignment is available"
    self.submissions.update_attributes({created_at: self.due_date - 1.minute,
                                        updated_at: self.due_date - 1.minute})
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
    UsedSub.find_by(user_id: user.id, assignment_id: self.id)&.submission
  end

  def main_submissions
    used_subs.map do |sfg|
      sfg.submission
    end
  end

  def graders_ordered
    graders.order("assignment_graders.order")
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

  def fixup_graders
    set_exam_graders if self.type == "exam"
    set_questions_graders if self.type == "questions"
  end

  def set_exam_graders
    # FIXME: This is complicated, and shouldn't be here either.

    upload = @assignment_file_data
    if upload.nil?
      if self.assignment_upload.nil?
        self.errors.add(:base, "Exam questions file is missing")
        return false
      else
        return true
      end
    else
      begin
        questions = YAML.load(upload.tempfile)
        upload.rewind
      rescue Psych::SyntaxError => e
        self.errors.add(:base, "Could not parse the supplied file")
        return false
      end
    end
    if !questions.is_a? Array
      self.errors.add(:base, "Supplied file does not contain a list of questions")
      return false
    else
      no_problems = true
      total_weight = 0
      def make_err(msg)
        self.errors.add(:base, msg)
        no_problems = false
      end
      def is_float(val)
        Float(val) rescue false
      end
      def is_int(val)
        Integer(val) rescue false
      end
      questions.each_with_index do |q, q_num|
        if q["parts"].is_a? Array
          q["parts"].each_with_index do |part, p_num|
            if !is_float(part["weight"])
              make_err "Question #{part['name']} has an invalid weight"
              next
            elsif !part["extra"]
              total_weight += Float(part["weight"])
            end
          end
        elsif !is_float(q["weight"])
          make_err "Question #{q['name']} has an invalid weight"
          next
        elsif !q["extra"]
          total_weight += Float(q["weight"])
        end
      end
      return unless no_problems
      self.graders ||= []
      if self.graders.count == 0
        self.graders << Grader.new(type: "ExamGrader", avail_score: total_weight)
      end
    end
  end

  # setup graders
  def set_questions_graders
    # FIXME: This is way too complicated.

    upload = @assignment_file_data
    if upload.nil?
      if self.assignment_upload.nil?
        self.errors.add(:base, "Assignment questions file is missing")
        return false
      else
        return true
      end
    else
      begin
        questions = YAML.load(upload.tempfile)
        upload.rewind
      rescue Psych::SyntaxError => e
        self.errors.add(:base, "Could not parse the supplied file")
        return false
      end
    end
    if !questions.is_a? Array
      self.errors.add(:base, "Supplied file does not contain a list of sections")
      return false
    else
      question_count = 0
      total_weight = 0
      question_kinds = Assignment.question_kinds.keys
      no_problems = true
      def make_err(msg)
        self.errors.add(:base, msg)
        no_problems = false
      end
      def is_float(val)
        Float(val) rescue false
      end
      def is_int(val)
        Integer(val) rescue false
      end
      questions.each_with_index do |section, index|
        if !(section.is_a? Object) || !(section.keys.is_a? Array) || section.keys.count > 1
          make_err "Section #{index} is malformed"
          next
        else
          section.each do |secName, sec_questions|
            sec_questions.each do |question|
              question.each do |type, q|
                question_count += 1
                begin
                  if !(type.is_a? String)
                    make_err "Question #{question_count} (in section #{secName}) has unknown type #{type}"
                    next
                  elsif !question_kinds.member?(type.underscore)
                    make_err "Question #{question_count} (in section #{secName}) has unknown type #{type}"
                    next
                  else
                    if q["weight"].nil? or !(Float(q["weight"]) rescue false)
                      make_err "Question #{question_count} has missing or invalid weight"
                    end
                    total_weight += Float(q["weight"])
                    ans = q["correctAnswer"]
                    if ans.nil?
                      make_err "Question #{question_count} is missing a correctAnswer"
                    end
                    if q["rubric"].nil?
                      make_err "Question #{question_count} is missing a rubric"
                    elsif !(q["rubric"].is_a? Array)
                      make_err "Question #{question_count} has an invalid rubric"
                    else
                      q["rubric"].each_with_index do |guide, i|
                        if !(guide.is_a? Object) or guide.keys.count != 1
                          make_err "Question #{question_count}, rubric entry #{i} is ill-formed"
                        else
                          guide.each do |weight, hint|
                            if !(Float(weight) rescue false)
                              make_err "Question #{question_count}, rubric entry #{i} has non-numeric weight"
                            elsif Float(weight) < 0 or Float(weight) > 1
                              make_err "Question #{question_count}, rubric entry #{i} has out-of-bounds weight"
                            end
                          end
                        end
                      end
                    end
                    if q["prompt"].nil?
                      make_err "Question #{question_count} is missing a prompt"
                    end
                    case type
                    when "YesNo", "TrueFalse"
                      if ![true, false].member?(q["correctAnswer"])
                        make_err "Boolean question #{question_count} has a non-boolean correctAnswer"
                      end
                    when "Numeric"
                      min = q["min"]
                      max = q["max"]
                      if max.nil? or !is_float(min)
                        make_err "Numeric question #{question_count} has a non-numeric max"
                      else
                        max = max.to_f
                      end
                      if min.nil? or !is_float(min)
                        make_err "Numeric question #{question_count} has a non-numeric min"
                      else
                        min = min.to_f
                      end
                      if ans.nil? or !is_float(ans)
                        make_err "Numeric question #{question_count} has a non-numeric ans"
                      else
                        ans = ans.to_f
                      end
                      if is_float(min) and is_float(max) and is_float(ans) and !(min <= ans and ans <= max)
                        make_err "Numeric question #{question_count} has a correctAnswer outside the specified range"
                      end
                    when "MultipleChoice"
                      if q["options"].nil? or !q["options"].is_a? Array
                        make_err "MultipleChoice question #{question_count} is missing an array of choices"
                      end
                      if !is_int(ans)
                        make_err "MultipleChoice question #{question_count} has a non-numeric correctAnswer"
                      else
                        ans = ans.to_i
                      end
                      if is_int(ans) and (ans < 0 or ans >= q["options"].count)
                        make_err "MultipleChoice question #{question_count} has a correctAnswer not in the available choices"
                      end
                    end
                    if q["parts"]
                      if !q["parts"].is_a? Array
                        make_err "Question #{question_count} has a non-list of parts"
                      else
                        q["parts"].each_with_index do |part, part_i|
                          if !part.is_a? Object
                            make_err "Question #{question_count} has a non-object part ##{part_i + 1}"
                          elsif part.keys.count > 1
                            make_err "Question #{question_count} part ##{part_i + 1} has too many keys"
                          elsif !["codeTag", "codeTags", "requiredText", "text"].member?(part.keys[0])
                            make_err "Question #{question_count} part ##{part_i + 1} has an invalid type #{part.keys[0]}"
                          end
                        end
                      end
                    end
                  end
                rescue Exception => e
                  make_err "Question #{question_count} in section #{secName} could not be parsed: #{e}"
                end
              end
            end
          end
        end
      end
      return unless no_problems
      self.graders ||= []
      if self.graders.count == 0
        self.graders << Grader.new(type: "QuestionsGrader", avail_score: total_weight)
      end
    end
  end
end
