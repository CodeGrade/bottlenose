class Section < ApplicationRecord
  def self.inheritance_column
    nil
  end
  enum type: [:lecture, :lab, :recitation, :online]

  belongs_to :course
  belongs_to :instructor, :class_name => "User", :foreign_key => "instructor_id", :primary_key => "id"
  delegate :term, to: :course

  validates :crn, presence: true, uniqueness: true # TODO: scope it per term?
  validates :instructor, presence: true
  validates :meeting_time, length: { minimum: 3 }

  after_save do
    if self.saved_change_to_crn?
      RegistrationSection.where(section_id: self.crn_before_last_save).update(section_id: self.crn)
      RegRequestSection.where(section_id: self.crn_before_last_save).update(section_id: self.crn)
    end
  end

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
    self.instructor = User.find_by_username(username)
  end
end
