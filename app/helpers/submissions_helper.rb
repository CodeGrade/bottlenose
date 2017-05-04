module SubmissionsHelper
  def student_names_for_select(students)
    options_for_select(students.map{|s| [s.id, s.name]})
  end
end
