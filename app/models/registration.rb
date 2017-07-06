class Registration < ApplicationRecord
  # The role for a registration is a way for Bottlenose to allow a single user
  # to be a staff role for one course, while being a student in another.
  # Professors have extra privileges over assistants.
  enum role: [:student, :grader, :assistant, :professor]

  validates_presence_of :course_id, :user_id
  validates_inclusion_of :show_in_lists, in: [false, true]
  
  # A registration is join model between a user and a course.
  belongs_to :user
  belongs_to :course

  has_one :term, through: :course
  has_many :registration_sections, dependent: :destroy
  has_many :sections, through: :registration_sections

  Section::types.each do |t, value|
    # This reader is for convenience
    define_method "#{t}_sections".to_sym do
      self.sections.where(type: value)
    end
  end

  attr_accessor :username
  attr_accessor :orig_sections
  attr_accessor :new_sections
  def after_initialize
    @orig_sections ||= []
    @new_sections ||= []
  end
  before_validation do
    if @username.blank?
      self.errors.add(:username, "cannot be blank") if self.user_id.nil?
    else
      uu = User.find_by(username: @username)
      if uu.nil?
        begin
          # TODO: Check LDAP for user.
          res = Devise::LDAP::Adapter.get_ldap_entry(username)
          if res
            uu = User.create!(username: username,
                              name: res[:displayname][0],
                              last_name: res[:sn][0],
                              first_name: res[:givenname][0],
                              email: res[:mail][0])
          end
        rescue => e
          self.errors.add(:base, "Could not find user with username #{username}: #{e}")
        end
      end
      self.user_id = uu&.id
    end
    if self.show_in_lists.nil?
      self.show_in_lists = (self.role == "student")
    end
  end
  def save_sections
    # should only be called if sections have been explicitly set
    if !@orig_sections.blank?
      self.errors.add(:base, "A new registration shouldn't have any sections to be removed")
    end
    if @new_sections.blank?
      self.errors.add(:base, "A new registration must include at least one section")
    end
    remove_old_sections
    create_new_sections
    return self.errors.size == 0
  end
  
  # Only one registration per user per course is allowed.
  validates :user_id, uniqueness: { scope: :course_id }

  # Return true if the registration is a staff role (not a student)
  def staff?
    (self.role == 'professor' || self.role == 'assistant' || self.role == 'grader') && self.dropped_date.nil?
  end

  def professor?
    self.role == 'professor'
  end

  # Return the submissions to the course the user is registered for.
  def submissions
    user.submissions.select { |s| s.course == course }
  end

  private
  def remove_old_sections
    @orig_sections&.each do |crn|
      rs = RegistrationSection.find_by(registration: self, section_id: crn)
      if rs.nil?
        self.errors.add(:base, "Could not find section #{crn} to remove enrollment")
      else
        begin
          rs.destroy!
        rescue Exception => e
          self.errors.add(:base, "Could not remove enrollment from section #{crn}: #{e}")
        end
      end
    end
  end
  def create_new_sections
    @new_sections&.each do |crn|
      s = Section.find_by(crn: crn)
      if s.nil?
        self.errors.add(:base, "Could not find section #{crn}")
      else
        begin
          RegistrationSection.find_or_create_by!(registration: self, section_id: crn)
        rescue Exception => e
          self.errors.add(:base, "Could not register for section #{crn}: #{e}")
        end
      end
    end
  end
end
