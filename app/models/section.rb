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


  validates :crn, presence: true
  validates :instructor, presence: true
  validates :meeting_time, length: { minimum: 3 }

  def to_s
    if self.meeting_time.to_s != ""
      "#{self.crn} : #{self.instructor.last_name} at #{self.meeting_time} (#{self.type.humanize})"
    else
      "#{self.crn} : #{self.instructor.last_name} (#{self.type.humanize})"
    end
  end

  def prof_name
    instructor&.username
  end

  def prof_name=(username)
    self.instructor = User.find_by(username: username)
  end
end
