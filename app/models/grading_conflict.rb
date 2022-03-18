class GradingConflict < ApplicationRecord
  
  # A GradingConflict is either not considered in allocations (inactive),
  # used in allocating staff graders to students (active), or has been
  # requested by a student and is awaiting approval (pending).

  # A student creates a Pending request. It can be marked Active by a professor, 
  # or Rejected completely. An Active conflict can be made Inactive and from
  # there can be made Pending again.
  
  # The activity history of a GradingConflict is recorded in the GradingConflictAudit table,
  # and an inactive request that becomes pending again cannot be rejected (i.e., the history
  # will not be deleted). See app/models/concerns/graiding_conflict_status.rb for implementation.
  enum status: [:active, :inactive, :pending, :rejected]

  belongs_to :staff, class_name: "User"
  belongs_to :student, class_name: "User"
  belongs_to :course

  has_many :grading_conflict_audits, -> { order(:created_at) }, dependent: :destroy

  validates :staff, presence: true
  validates :student, presence: true
  validates :course, presence: true
  validate :staff_and_student_different_users
  validate :users_are_registered_for_course
  validate :users_must_have_correct_roles

  def can_be_rejected?
    return self.pending? && self.grading_conflict_audits.count <= 1
  end

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
