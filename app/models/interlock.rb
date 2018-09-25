class Interlock < ApplicationRecord
  belongs_to :assignment
  belongs_to :related_assignment, class_name: "Assignment"
  enum constraint: [:no_submission_unless_submitted,
                    :no_submission_after_viewing,
                    :check_section_toggles]

  def to_s
    if self.constraint == "check_section_toggles"
      Interlock.constraint_to_s(self.constraint)
    else
      "#{Interlock.constraint_to_s(self.constraint)} #{self.related_assignment.name}"
    end
  end
  
  def self.constraint_to_s(con)
    {no_submission_unless_submitted: "Prohibit submission unless submitted to",
     no_submission_after_viewing: "Submissions are prohibited after viewing",
     check_section_toggles: "Submissions are locked down by section"}[con.to_sym]
  end
end
