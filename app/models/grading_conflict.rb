class GradingConflict < ApplicationRecord

  belongs_to :staff, class_name: "User"
  belongs_to :student, class_name: "User"
  belongs_to :course

  # TODO: What else needs to be validated?
  validates :staff, presence: true
  validates :student, presence: true
  validates :course, presence: true
  validate :staff_and_student_different_users
  validate :users_are_registered_for_course
  validate :users_must_have_correct_roles

  def users_are_registered_for_course
    if student.registration_for(course).nil? 
      errors.add(:student, "must be registered for course.")
    end
    if staff.registration_for(course).nil? 
      errors.add(:staff, "must be registered for course.")
    end
  end

  def users_must_have_correct_roles
    unless self.users_have_proper_roles?
      errors.add(:base, "GradingConflict's staff user is not a grader OR student user is not a student.")
    end
  end

  def staff_and_student_different_users
    if staff == student
      errors.add(:base, "Staff and student must be different people.")
    end
  end

  private

  def users_have_proper_roles?
    return (self.staff.course_grader?(course) || 
      self.staff.course_assistant?(course)) &&
      self.student.course_student?(course)
  end
end
