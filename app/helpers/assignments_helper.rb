module AssignmentsHelper
  def join(arr, between, before_last=nil)
    ans = ""
    (0..arr.length - 1).each do |i|
      if i > 0
        if i == arr.length - 1 && !before_last.nil?
          ans += before_last
        else
          ans += between
        end
      end
      ans += arr[i].to_s
    end
    ans
  end
  
  def interlock_confirmation(asgn)
    nsavs = asgn.related_interlocks.where(constraint: Interlock::constraints[:no_submission_after_viewing])
    if nsavs.empty?
      {}
    else
      constraints = join(nsavs.map{|i| i.assignment.name}, ", ", " or ")
      { confirm:
          ("Once you view the questions on this assignment, " +
           "you are not allowed to resubmit to #{constraints}. " +
           "Are you sure you want to continue?") }
    end
  end
end
