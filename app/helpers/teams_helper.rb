module TeamsHelper
  def names_for_select(users)
    options_for_select(users.map {|uu| [uu.name, uu.id] })
  end
end
