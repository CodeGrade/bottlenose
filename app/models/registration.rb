class Registration < ApplicationRecord
  # The role for a registration is a way for Bottlenose to allow a single user
  # to be a staff role for one course, while being a student in another.
  # Professors have extra privileges over assistants.
  enum role: [:student, :grader, :assistant, :professor]

  # A registration is join model between a user and a course.
  belongs_to :user
  belongs_to :course

  has_one :term, through: :course
  belongs_to :section, class_name: "CourseSection", :foreign_key => :section_id, :primary_key => "crn"

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
end
