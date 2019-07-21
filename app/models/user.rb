require 'securerandom'
require 'audit'
require 'open3'

class User < ApplicationRecord
  MAGIC = {png: ["\x89PNG\x0d\x0a\x1a\x0a"],
           jpg: ["\xff\xd8\xff\xdb", "\xff\xd8\xff\xe0", "\xff\xd8\xff\xe1",
                 "\xff\xd8\xff\xe2", "\xff\xd8\xff\xe3", "\xff\xd8\xff\xe8", "\xff\xd8\xff\xed"],
           gif: ["GIF87a", "GIF89a"],
           bmp: ["BM"]}
  # Include default devise modules. Others available are:
  # :confirmable, :lockable, :timeoutable and :omniauthable
  devise :ldap_authenticatable, :database_authenticatable,
         :recoverable, :rememberable, :trackable, :registerable 
  has_many :registrations, dependent: :destroy
  has_many :courses, through: :registrations
  has_many :sections, through: :registrations

  has_many :user_submissions, dependent: :destroy
  has_many :submissions, through: :user_submissions
  has_many :reg_requests, dependent: :destroy

  has_many :individual_extensions, dependent: :destroy

  has_many :team_users, dependent: :restrict_with_error
  has_many :teams, through: :team_users
  has_many :team_requests, dependent: :destroy

  validates :email, :format => { :with => /\@.*\./ }, :allow_nil => true

  validates :name,  length: { in: 2..60 }
  validates :nuid,
            numericality: {only_integer: true,
                           less_than_or_equal_to: 1999999999,
                           greater_than_or_equal_to: 0},
            :allow_blank => true,
            :uniqueness => true

  validate :profile_is_image
  before_update :make_profile_thumbnail
  
  def self.pepper
    Devise.pepper
  end

  def self.stretches
    Devise.stretches
  end

  def email_changed?
    false
  end

  attr_accessor :nuid_safe
  def nuid_safe(who)
    if who.nil?
      nil
    elsif who.id == self.id
      self.nuid
    elsif who.professor_ever?
      self.nuid
    else
      nil
    end
  end

  def profile_path
    Upload.full_path_for(self.profile)
  end
  def profile_path=(val)
    self.profile = Upload.upload_path_for(val)
  end
  def profile_image_type
    start = IO.read(self.profile_path, 8)
    MAGIC.each do |type, magics|
      magics.each do |m|
        if start.bytes[0..(m.length - 1)] == m.bytes
          return type
        end
      end
    end
    return false
  end
  def profile_is_image
    if self.profile_path && File.file?(self.profile_path)
      if !profile_image_type
        self.errors.add(:base, "Profile file matches no known image file type")
        return false
      end
      return true
    end
  rescue Exception => e
    self.errors.add(:base, "Could not read profile file #{Upload.upload_path_for(profile)}: #{e}")
    return false
  end
  
  def make_profile_thumbnail(force=false)
    if (force || self.profile_changed?) && self.profile_path && File.file?(self.profile_path)
      old_file = self.profile_path
      secret = SecureRandom.urlsafe_base64
      new_file = Upload.base_upload_dir.join("#{secret}_#{self.username}_profile_thumb.jpg")
      type = profile_image_type
      Audit.log "Making #{new_file} from #{type} #{old_file}"
      output, err, status = Open3.capture3("convert",
                                           "#{type}:#{old_file}",
                                           "-resize", "200x300",
                                           "-delete", "1--1", # ignore all but the first frame of gifs
                                           "-define", "jpeg:extent=50KB",
                                           "-auto-orient",
                                           "jpg:#{new_file}")
      if status.success?
        self.profile_path = new_file
        self.save! if force
        FileUtils.rm(old_file)
        return true
      else
        self.errors.add(:base,
                        "Could not create a thumbnail of the profile #{Upload.upload_path_for(old_file)}: #{output}, #{err}")
        return false
      end
    end
  rescue Exception => e
    self.errors.add(:base, "Could not create a thumbnail of the profile #{Upload.upload_path_for(old_file)}: #{e}")
    return false
  end

  # Different people with the same name are fine.
  # If someone uses two emails, they get two accounts. So sad.

  def ldap_before_save
    res = Devise::LDAP::Adapter.get_ldap_entry(self.username)
    return unless res
    self.name = res[:displayname][0]
    if res[:sn]
      self.last_name = res[:sn][0]
    end
    if res[:givenname]
      self.first_name = res[:givenname][0]
    end
    if res[:mail]
      self.email = res[:mail][0]
    end
  end

  def sort_name
    if self.first_name && self.last_name
      "#{self.last_name}, #{self.first_name}"
    else
      self.name
    end
  end

  def display_name
    if self.first_name && self.last_name
      if !self.nickname.blank? && self.nickname != self.first_name
        "#{self.first_name} (#{self.nickname}) #{self.last_name}"
      else
        "#{self.first_name} #{self.last_name}"
      end
    else
      self.name
    end
  end

  before_validation do
    unless self.email.nil?
      self.email = self.email.downcase
      self.email = self.email.strip
      self.email.sub!(/\W$/, '')
    end
  end

  def to_s
    self.email
  end

  def late_days_used(assignments)
    self.used_submissions_for(assignments).reduce(0) do |acc, s|
      acc + s.submission.days_late
    end
  end

  def submissions_for(assn)
    assn.submissions_for(self)
  end

  def used_submissions_for(assignments)
    if assignments.is_a? Array
      assn_ids = assignments.map(&:id)
    elsif assignments.is_a? Assignment
      assn_ids = assignments.id
    else
      assn_ids = assignments.pluck(:id)
    end
    UsedSub.where(user: self, assignment_id: assn_ids).joins(:submission)
  end

  def course_staff?(course)
    return false if course.nil?
    reg = registration_for(course)
    return false if reg.nil?
    reg.role == "professor" || reg.role == "assistant" || reg.role == "grader"
  end

  def self.professors
    User.joins(:registrations).where("registrations.role": Registration::roles["professor"]).distinct.order(:id)
  end
  
  def professor_ever?
    Registration.where(user_id: self.id, role: Registration::roles["professor"]).count > 0
  end

  def course_professor?(course)
    course.registered_by?(self, as: 'professor')
  end

  def course_assistant?(course)
    course.registered_by?(self, as: 'assistant')
  end

  def course_grader?(course)
    course.registered_by?(self, as: 'grader')
  end

  def course_student?(course)
    course.registered_by?(self, as: 'student')
  end

  def registration_for(course)
    Registration.find_by(user_id: self.id, course_id: course&.id)
  end

  def invert_name
    name.split(/\s+/).rotate(-1).join(' ')
  end

  def surname
    invert_name.split(/\s+/).first
  end

  def dir_name
    invert_name.gsub(/\W/, '_')
  end

  def reasonable_name?
    name =~ /\s/ && name.downcase != name
  end

  def active_team_for(course, assn)
    @active_team ||= teams_for(course).where(teamset_id: assn.teamset_id).select(&:active?).first
  end

  def teams_for(course)
    @teams ||= teams.where(course: course)
  end

  def grouped_registrations
    ret = {}
    regs = self.registrations
    reg_sections = RegistrationSection.where(registration_id: regs.map(&:id))
    sections = Section.where(id: reg_sections.map(&:section_id)).map{|s| [s.id, s]}.to_h
    reg_sections = reg_sections.group_by(&:registration_id)
    terms = Term.all_sorted.to_a
    all_regs_by_term = regs.joins(:course).select("registrations.*", "courses.term_id as term_id").group_by(&:term_id)
    courses = Course.where(id: regs.map(&:course_id)).map{|c| [c.id, c]}.to_h
    Registration.roles.each do |role_name, role_val|
      by_role = ret[role_name]
      if by_role.nil? then by_role = ret[role_name] = {} end
      if by_role[:count].nil? then by_role[:count] = 0 end
      terms.each do |term|
        regs_by_term = all_regs_by_term[term.id]
        by_term = by_role[term.name]
        if by_term.nil? then by_term = by_role[term.name] = [] end
        regs_by_term&.select{|r| r.role == role_name}&.each do |r|
          r_sections = sections.values_at(*reg_sections[r.id].map(&:section_id))
          by_term.push({course: courses[r.course_id],
                        sections: r_sections.group_by(&:type),
                        withdrawn: r.dropped_date})
          by_role[:count] += 1
        end
      end
    end
    ret
  end

  def disconnect(course = nil)
    # Dissolve any teams, if this person is a student in the course
    if course
      to_dissolve = teams.where(course_id: course.id, end_date: nil)
    else
      to_dissolve = teams.where(end_date: nil)
    end
    to_dissolve.each do |t| t.dissolve(DateTime.current) end
    # Abandon any grading, if this person is a grader for the course
    # Note: it seems like `who_grades` doesn't work with objects, so I need to use `who_grades_id` here
    GraderAllocation.where(course: course, who_grades_id: self.id, grading_completed: nil)
      .update_all(abandoned: true)
  end

  def course_regs_by_term
    regs = self.registrations.includes(:course).includes(:term)
    roles = regs.map(&:role)
    courses = regs.map(&:course)
    rs = RegistrationSection.where(registration_id: regs.map(&:id)).includes(:section).group_by(&:registration_id)
    sections = regs.map do |r|
      rs[r.id].map(&:section)
    end
    dropped = regs.map(&:dropped_date)
    terms = courses.map(&:term)
    multi_group_by(terms.zip(courses, sections, roles, dropped), [:first, :second], true)
  end

  def active_courses
    # sort descending on terms, ascending on names, grouped by term name
    self.courses.includes(:term).where('terms.archived': false)
      .map{|c| [c.term, c]}.group_by(&:first).to_a
      .sort{|(t1, c1s), (t2, c2s)| t2.canonical_name <=> t1.canonical_name}
      .map{|t, cs| [t.name, cs.map(&:second).sort_by(&:name)]}.to_h
  end
end
