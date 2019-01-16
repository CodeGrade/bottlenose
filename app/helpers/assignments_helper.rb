require 'schema_checker'
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

  def check_questions_schema
    upload = @assignment_file_data
    begin
      case [upload.nil?, self.assignment_upload.nil?]
      when [false, true] # New assignment
        @old_weights = nil
        @questions = YAML.load(upload.read)
        upload.rewind
      when [false, false] # Updated assignment
        old_questions = YAML.load(File.read(self.assignment_upload.submission_path))
        @old_weights = old_questions.map{|section| section.map{|_, qs| qs.map{|q| Float(q.first[1]["weight"])}}}
        @questions = YAML.load(upload.read)
        upload.rewind
      when [true, false] # Existing assignment being re-checked
        @old_weights = nil
        @questions = YAML.load(File.read(self.assignment_upload.submission_path))
      when [true, true] # Impossible
        self.errors.add(:base, "Assignment questions file is missing")
        return false
      end
    rescue Psych::SyntaxError => e
      self.errors.add(:base, "Could not parse the supplied file: #{e}")
      return false
    end
    sc = SchemaChecker.new(Rails.root.join("app/helpers/questions_schema.yaml"))
    sc.check(@questions).each{|e| self.errors.add(:base, e)}      
    return false if self.errors.count > 0
    @questions = sc.convert(@questions)
    @weights = @questions.map{|section| section.map{|_, qs| qs.map{|q| q.first[1]["weight"]}}}
    @total_weight = @weights.flatten.sum
    return true
  end
end
