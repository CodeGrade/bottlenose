class FixedDaysConfig < LatenessConfig
  validates :days_per_assignment, :numericality => true
  validates :max_penalty, :numericality => true

  def allow_submission?(assignment, submission)
    late_days = days_late(assignment, submission)
    return true if late_days == 0
    return false if late_days > self.days_per_assignment
    max_late_days = assignment.course.total_late_days
    return true if max_late_days.nil?
    # NOTE: Cannot use .users here because the assignment isn't yet submitted
    # so it doesn't show up in the UserSubmissions relation
    # NOTE: Cannot just use assignment.course.assignments because the student might've
    # submitted late to this assignment already, so don't re-penalize that late submission
    all_but_current = assignment.course.assignments.where.not(id: assignment.id)
    below_max_days_limit = submission.submission_users.all? do |u|
      (u.late_days_used(all_but_current) + late_days <= max_late_days)
    end
    below_max_assignments_limit = submission.submission_users.all? do |u|
      u.used_submissions_for(all_but_current).map(&:submission).select(&:late?).size < self.max_penalty
    end
    below_max_days_limit && below_max_assignments_limit
  end

  def late_penalty(assignment, submission)
    0
  end

  def to_s
    "Allow #{self.days_per_assignment} days per assignment; limit to course's maximum total late days; everyone on team must have sufficient late days remaining"
  end

  def ==(other)
    if other.instance_of?(FixedDaysConfig)
      self.days_per_assignment == other.days_per_assignment &&
        self.max_penalty == other.max_penalty
    else
      false
    end
  end
end
