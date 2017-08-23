class Files < Assignment
  validates :related_assignment_id, :absence => true
  
  def questions
    nil
  end
  def flattened_questions
    nil
  end
end
