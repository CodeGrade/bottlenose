require 'schema_checker'
class Exam < Assignment
  validates :related_assignment_id, :absence => true
  validate :set_exam_graders
  before_update :update_submissions_if_needed
  before_update :update_exam_submission_times

  def assign_attributes(attrs)
    @exam_disposal = attrs.delete(:exam_disposal)
    super(attrs)
    self.available = self.due_date
    self.lateness_config = self.course.lateness_config
  end
  
  def set_exam_graders
    return true unless (self.new_record? || self.assignment_upload_id_changed?)
    upload = @assignment_file_data
    if upload.nil?
      if self.assignment_upload.nil?
        self.errors.add(:base, "Exam questions file is missing")
        return false
      else
        return true
      end
    else
      begin
        questions = YAML.load(upload.read)
        upload.rewind
      rescue Psych::SyntaxError => e
        self.errors.add(:base, "Could not parse the supplied file")
        return false
      end
    end
    @total_weight = 0
    sc = SchemaChecker.new(Rails.root.join("app/helpers/exams_schema.yaml"))
    sc.check(questions).each{|e| self.errors.add(:base, e)}
    return false if self.errors.count > 0
    questions = sc.convert(questions)
    weights = questions.map{|q| q["parts"] || q}.flatten.select{|q| !q["extra"]}.map{|q| q["weight"]}
    @total_weight = weights.sum
    grader = self.graders.first
    if grader.nil?
      grader = Grader.new(type: "ExamGrader", assignment: self)
      self.graders << grader
    end
    grader.avail_score = @total_weight
    grader.order = self.graders.count + 1
  end

  def update_submissions_if_needed
    return if @inSave
    if @assignment_file_data.nil?
      debugger
    end
    begin
      @new_questions = YAML.load(@assignment_file_data.read)
      @assignment_file_data.rewind
    rescue Psych::SyntaxError => e
      self.errors.add(:base, "Could not parse the supplied file")
      raise ActiveRecord::Rollback
    end
    return if self.used_subs.empty?
    @old_questions = questions(self.assignment_upload.submission_path)
    @assignment_file_data.rewind
    @old_flat = self.flattened_questions(@old_questions)
    @new_flat = self.flattened_questions(@new_questions)
    case @exam_disposal
    when "delete"
      self.submissions.destroy_all
    when "percentage"
      inconsistency = questions_incompatible(@old_questions, @new_questions)
      if inconsistency
        self.errors.add(:base, inconsistency)
        raise ActiveRecord::Rollback
      end
      sub_ids = self.used_subs.select(:submission_id)
      @old_flat.zip(@new_flat).each_with_index do |(old_q, new_q), i|
        factor = new_q["weight"].to_f / old_q["weight"].to_f
        InlineComment.where(submission_id: sub_ids, line: i).update_all("weight = weight * #{factor}")
      end
    when "points"
      inconsistency = questions_incompatible(@old_questions, @new_questions)
      if inconsistency
        self.errors.add(:base, inconsistency)
        raise ActiveRecord::Rollback
      end
      inconsistency = weights_too_low(@new_flat)
      if inconsistency
        self.errors.add(:base, inconsistency)
        raise ActiveRecord::Rollback
      end
      # Nothing to be done, because points are treated as absolute
    else
      raise "Unexpected exam disposal option: #{@exam_disposal}"
    end

    weights = @new_flat.select{|q| !q["extra"]}.map{|q| q["weight"]}
    @total_weight = weights.sum
    grader = self.graders.first
    if grader.nil?
      self.errors.add(:base, "No grader was found for this exam.")
      raise ActiveRecord::Rollback
    end
    grader.avail_score = @total_weight
    grader.expect_num_questions(@new_flat.count)
    self.used_subs.each do |u|
      grader.grade(self, u.submission)
    end
    @need_to_unpublish_grades = "force"
    grader.grades.update_all(out_of: @total_weight)
  end


  def set_need_to_unpublish_grades
    return if (@need_to_unpublish_grades == "force")
    @need_to_unpublish_grades =
      self.graders.any?{|g| g.new_record? || g.changed? || g.marked_for_destruction?}
  end
  
  def weights_too_low(flattened_new_qs)
    # Returns true if any existing grades are greater than the new maximum scores
    sub_ids = self.used_subs.select(:submission_id)
    flattened_new_qs.each_with_index do |q, i|
      if !InlineComment.where(submission_id: sub_ids, line: i).where('weight > ?', q["weight"]).empty?
        return "At least one grade on Question #{i} is greater than the new maximum score"
      end
    end
    return false
  end

  def questions_incompatible(old_qs, new_qs)
    return "Question count has changed" if old_qs.count != new_qs.count
    old_qs.zip(new_qs).each_with_index do |(old, new), q_num|
      if old["parts"].nil? != new["parts"].nil?
        return "Question #{q_num} has inconsistent subparts"
      elsif old["parts"]&.count != new["parts"]&.count
        return "Question #{q_num} has different numbers of subparts"
      end
    end
    return false
  end
  
  def update_exam_submission_times
    self.available = self.due_date # for exams, there's no window in which "the assignment is available"
    self.submissions.update_all({created_at: self.due_date - 1.minute,
                                 updated_at: self.due_date - 1.minute})
  end

  def questions(file = nil)
    return @questions if file.nil? && @questions
    @questions = YAML.load(File.read(file || self.assignment_upload.submission_path))
    @questions.each_with_index do |q, q_num|
      q["name"] = "Problem #{q_num + 1}" unless q["name"]
      if q["parts"]
        grade = 0
        q["parts"].each_with_index do |p, p_num|
          p["name"] = "Problem #{q_num + 1}#{('a'..'z').to_a[p_num]}" unless p["name"]
          grade += p["weight"]
        end
        q["weight"] = grade unless q["weight"]
      end
    end
    @questions
  end
  
  def flattened_questions(qs = nil)
    qs = qs || self.questions
    flat = []
    qs.each do |q|
      if q["parts"]
        q["parts"].each do |p|
          flat.push p
        end
      else
        flat.push q
      end
    end
    flat
  end
  
  def sections
    [{name: "", count: self.flattened_questions.count}]
  end
end
