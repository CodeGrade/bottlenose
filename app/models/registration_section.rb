class RegistrationSection < ApplicationRecord
  belongs_to :registration
  belongs_to :section, :foreign_key => "section_id", :primary_key => "crn"

  validate do
    if self.section.course_id != self.registration.course_id
      self.errors.add(:base, "Registration's course is for #{self.registration.course.name} (id #{self.registration.course_id}), but the section belongs to #{self.section.course.name} (id #{self.section.course_id})")
    end
  end
end
