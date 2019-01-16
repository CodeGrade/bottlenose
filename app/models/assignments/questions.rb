class Questions < Assignment
  include AssignmentsHelper
  validates :related_assignment_id, :absence => true
  validate :set_questions_graders

  def set_questions_graders
    return false unless check_questions_schema
    grader = self.graders.first
    if grader.nil?
      grader = Grader.new(type: "QuestionsGrader", assignment: self)
      self.graders << grader
    end
    grader.avail_score_will_change! if (@old_weights != @weights)
    grader.avail_score = @total_weight
    grader.order = self.graders.count + 1
  end

  def questions
    return @questions if @questions
    @questions = YAML.load(File.read(self.assignment_upload.submission_path))
    @questions
  end

  def flattened_questions
    qs = self.questions
    flat = []
    qs.each do |section|
      section.each do |name, qs|
        qs.each do |question|
          question.each do |type, q|
            q["type"] = type;
            q["sectionName"] = name
            flat.push q
          end
        end
      end
    end
    flat
  end

  def sections
    self.questions.map do |section|
      {name: section.to_a[0][0], count: section.to_a[0][1].count}
    end
  end
end
