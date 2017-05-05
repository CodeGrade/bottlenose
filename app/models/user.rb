require 'securerandom'
require 'audit'

class User < ActiveRecord::Base
  # Include default devise modules. Others available are:
  # :confirmable, :lockable, :timeoutable and :omniauthable
  devise :ldap_authenticatable, :database_authenticatable,
         :recoverable, :rememberable, :trackable, :registerable 
  has_many :courses, through: :registrations
  has_many :registrations, dependent: :destroy

  has_many :submissions, through: :user_submissions
  has_many :user_submissions, dependent: :destroy
  has_many :reg_requests, dependent: :destroy

  has_many :teams, through: :team_users
  has_many :team_users, dependent: :restrict_with_error

  validates :email, :format => { :with => /\@.*\./ }, :allow_nil => true

  validates :name,  length: { in: 2..30 }
  validates :nuid, numericality: {only_integer: true}, :allow_blank => true

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

  def active_team_for(course)
    @active_team ||= teams_for(course).select(&:active?).first
  end

  def teams_for(course)
    @teams ||= teams.where(course: course)
  end

  def grouped_registrations
    ret = {}
    regs = self.registrations
    terms = Term.all_sorted.to_a
    Registration.roles.each do |role_name, role_val|
      by_role = ret[role_name]
      if by_role.nil? then by_role = ret[role_name] = {} end
      if by_role[:count].nil? then by_role[:count] = 0 end
      terms.each do |term|
        regs_by_term = regs.joins(:course).where("courses.term_id": term.id).to_a
        by_term = by_role[term.name]
        if by_term.nil? then by_term = by_role[term.name] = [] end
        regs_by_term.select{|r| r.role == role_name}.each do |r|
          by_term.push(r.course)
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
    regs = self.registrations.includes(:course).includes(:section).includes(:term)
    courses = regs.map(&:course)
    sections = regs.map(&:section)
    dropped = regs.map(&:dropped_date)
    terms = courses.map(&:term)
    terms.zip(courses, sections, dropped).group_by{|tcsd| tcsd[0]}
  end
end
