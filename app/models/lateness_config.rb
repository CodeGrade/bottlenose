require 'clamp'

class LatenessConfig < ApplicationRecord
  def self.unique
    select(column_names - ["id"]).distinct
  end

  def self.default
    LatePerDayConfig.new(days_per_assignment: 4, frequency: 1, max_penalty: 100, percent_off: 25)
  end

  def late?(assignment, submission)
    (!submission.ignore_late_penalty) and
      ((submission.created_at || DateTime.current) > assignment.due_date)
  end

  def allow_submission?(assignment, submission)
    fail NotImplementedError, "Each lateness config should implement this"
  end

  def late_penalty(assignment, submission)
    fail NotImplementedError, "Each lateness config should implement this"
  end

  def days_late(assignment, submission, raw = false)
    return 0 unless raw or late?(assignment, submission)
    due_on = assignment.due_date
    sub_on = submission.created_at || DateTime.current
    late_days = ((sub_on.to_f - due_on.to_f) / 1.day.seconds)
    late_days.ceil
  end

  def hours_late(assignment, submission, raw = false)
    return 0 unless raw or late?(assignment, submission)
    due_on = assignment.due_date
    sub_on = submission.created_at || DateTime.current
    late_hours = ((sub_on.to_f - due_on.to_f) / 1.hour.seconds)
    late_hours.ceil
  end

  def penalize(score, assignment, submission)
    # score is [0, 100]
    penalty = late_penalty(assignment, submission) # compute penalty in [0, 100]
    #print "Penalty is #{penalty}\n"
    if self.max_penalty # cap it
      penalty = penalty.clamp(0, self.max_penalty)
    end
    #print "Penalty is now #{penalty}\n"
    #print "Score is #{score}\n"
    ans = (score - penalty).clamp(0, 150)
    #print "Penalized score is #{ans}\n"
    ans
  end
end

