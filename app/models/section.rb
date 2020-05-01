class Section < ApplicationRecord
  def self.inheritance_column
    nil
  end
  enum type: [:lecture, :lab, :recitation, :online]

  belongs_to :course
  belongs_to :instructor, :class_name => "User", :foreign_key => "instructor_id", :primary_key => "id"
  delegate :term, to: :course
  has_many :registration_sections
  has_many :registrations, through: :registration_sections
  has_many :users, through: :registrations

  has_many :submission_enabled_toggles

  validates :crn, presence: true, uniqueness: { scope: :course_id, message: "%{value} already exists for this course" }
  validates :instructor, presence: true
  validates :meeting_time, length: { minimum: 3 }

  def to_s(show_instructor: true, show_time: true, show_type: true)
    ans = "#{self.crn}"
    ans = "#{ans} : #{self.instructor.last_name}" if show_instructor
    ans = "#{ans} at #{self.meeting_time}" if (show_time && self.meeting_time.to_s != "")
    ans = "#{ans} (#{self.type.humanize})" if show_type
    ans
  end

  def prof_name
    instructor&.username
  end

  def prof_name=(username)
    self.instructor = User.find_by(username: username)
  end

  def students
    users.where("registrations.role": Registration::roles["student"])
  end

  def students_with_registrations
    students.joins("JOIN registration_sections ON registrations.id = registration_sections.registration_id")
      .joins("JOIN sections ON registration_sections.section_id = sections.id")
  end

  def users_with_drop_info
    self.course.users_with_drop_info(self.users)
  end

  def students_with_drop_info
    self.course.users_with_drop_info(self.students)
  end

  def active_students
    self.students_with_drop_info.where("registrations.dropped_date": nil)
  end

  def professors
    users.where("registrations.role": Registration::roles["professor"])
  end

  def graders
    users.where("registrations.role": Registration::roles["grader"])
  end

  def staff
    users
      .where("registrations.role <> #{Registration::roles["student"]}")
      .where("registrations.dropped_date is null")
  end
end
