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
      ((submission.created_at || DateTime.current) > assignment.effective_sub_due_date(submission))
  end

  def allow_submission?(assignment, submission)
    fail NotImplementedError, "Each lateness config should implement this"
  end

  def late_penalty(assignment, submission)
    fail NotImplementedError, "Each lateness config should implement this"
  end

  def days_late(assignment, submission, raw = false)
    return 0 unless raw or late?(assignment, submission)
    due_on = assignment.effective_due_date(submission.user, submission.team)
    sub_on = submission.created_at || DateTime.current
    late_days = ((sub_on.to_f - due_on.to_f) / 1.day.seconds)
    late_days.ceil
  end

  def penalize(score, assignment, submission, max_pct = 100.0, max_pct_with_ec = 100.0)
    # score is [0, max_pct]
    penalty = late_penalty(assignment, submission) # compute penalty in [0, 100]
    #print "Penalty is #{penalty}\n"
    if self.max_penalty # cap it
      penalty = penalty.clamp(0, self.max_penalty)
    end
    penalty = penalty * (max_pct / 100.0) # scale to [0, max_pct]
    #print "Penalty is now #{penalty}\n"
    #print "Score is #{score}\n"
    ans = (score - penalty).clamp(0, max_pct_with_ec)
    #print "Penalized score is #{ans}\n"
    ans
  end
end

