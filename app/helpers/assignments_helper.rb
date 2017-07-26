module AssignmentsHelper
  def interlock_confirmation(asgn)
    if asgn.interlocks.empty?
      {}
    else
      interlock = asgn.interlocks.find_by(constraint: Interlock::constraints[:no_submission_after_viewing])
      if interlock
        { confirm:
            ("Once you view the questions on this assignment, " +
             "you are not allowed to resubmit to #{interlock.related_assignment.name}. " +
             "Are you sure you want to continue?") }
      else
        {}
      end
    end
  end
end
