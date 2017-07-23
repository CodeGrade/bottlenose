require 'securerandom'
require 'audit'
require 'open3'

class User < ApplicationRecord
  MAGIC = {png: ["\x89PNG\x0d\x0a\x1a\x0a"],
           jpg: ["\xff\xd8\xff\xdb", "\xff\xd8\xff\xe0", "\xff\xd8\xff\xe1"],
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

  has_many :team_users, dependent: :restrict_with_error
  has_many :teams, through: :team_users

  validates :email, :format => { :with => /\@.*\./ }, :allow_nil => true

  validates :name,  length: { in: 2..30 }
  validates :nuid, numericality: {only_integer: true}, :allow_blank => true

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

  def profile_image_type
    start = IO.read(self.profile, 8)
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
    if self.profile && File.file?(self.profile)
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

  def make_profile_thumbnail
    if self.profile_changed? && self.profile && File.file?(self.profile)
      old_file = self.profile
      secret = SecureRandom.urlsafe_base64
      new_file = Upload.base_upload_dir.join("#{secret}_#{self.username}_profile_thumb.jpg")
      type = profile_image_type
      Audit.log "Making #{new_file} from #{type} #{old_file}\n"
      output, err, status = Open3.capture3("convert",
                                           "#{type}:#{old_file}",
                                           "-resize", "200x300",
                                           "-define", "jpeg:extent=50KB",
                                           "jpg:#{new_file}")
      if status.success?
        self.profile = new_file
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
      if !self.nickname.to_s.empty?
        disp = "#{self.first_name} (#{self.nickname}) #{self.last_name}"
      else
        disp = "#{self.first_name} #{self.last_name}"
      end
    else
      disp = self.name
    end
  end

  def valid_ldap_authentication?(pwd)
    if !self.new_record? &&
       self.encrypted_password != "" &&
       Devise::Encryptor.compare(self.class, self.encrypted_password, pwd)
      Audit.log("DB auth for #{self.name}")
      true
    else
      super
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

  def professor_ever?
    Registration.where(user_id: self.id, role: RegRequest::roles["professor"]).count > 0
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
    Registration.find_by_user_id_and_course_id(self.id, course.id)
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
    sections = Section.where(crn: reg_sections.map(&:section_id)).map{|s| [s.crn, s]}.to_h
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
          by_term.push({course: courses[r.course_id], sections: r_sections.group_by(&:type)})
          by_role[:count] += 1
        end
      end
    end
    ret
  end

  def disconnect(course = nil)
    if course
      to_dissolve = teams.where(course_id: course.id, end_date: nil)
    else
      to_dissolve = teams.where(end_date: nil)
    end
    to_dissolve.each do |t| t.dissolve(DateTime.current) end
  end

  def course_regs_by_term
    regs = self.registrations.includes(:course).includes(:term)
    courses = regs.map(&:course)
    rs = RegistrationSection.where(registration_id: regs.map(&:id)).includes(:section).group_by(&:registration_id)
    sections = regs.map do |r|
      rs[r.id].map(&:section)
    end
    dropped = regs.map(&:dropped_date)
    terms = courses.map(&:term)
    terms.zip(courses, sections, dropped).group_by{|tcsd| tcsd[0]}
  end
end
