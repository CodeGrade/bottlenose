module GraderAllocationsHelper
  def names_for_submissions(subs)
    options_for_select(subs.map {|sub| [sub.submission_user_names, sub.id] })
  end
  def names_for_graders(graders)
    options_for_select(graders.map {|g| [g.name, g.id] })
  end
end
