require 'clamp'
require 'csv'
class ExamGrader < Grader
  # Overridden from Grader, because the default is to publish automatically
  def grade(assignment, sub)
    do_grading(assignment, sub)
    # and don't compute_grade automatically; wait to publish
  end

  def autograde!(assignment, sub, prio = 0)
    g = self.grade_for sub

    g.out_of = self.avail_score

    g.updated_at = DateTime.now
    g.available = false
    g.save!

    return nil
  end

  def to_s
    "Exam grading"
  end

  def display_type
    "Exam score"
  end

  def expect_num_questions(num)
    @num_questions = num
  end

  def upload_file=(data)
    unless data.nil?
      errors.add(:base, "You cannot submit a file for exams.")
      return
    end
  end

  def normal_weight
    self.assignment.flattened_questions.reject{|q| q["extra"]}.map{|q| q["weight"]}.sum
  end

  def extra_credit_weight
    self.assignment.flattened_questions.select{|q| q["extra"]}.map{|q| q["weight"]}.sum
  end

  def export_data
    out = Rails.root.join("private", "assignment_#{self.assignment_id}.csv")
    CSV.open(out, "wb") do |csv|
      csv << ["NUID", "Last name", "First name", "Status", "Action",
              *self.assignment.flattened_questions.map{|q| q["name"]},
              "Curved"]
      subs = self.assignment.used_submissions.includes(:users).map{|u| [u.user_id, u]}.to_h
      grades = self.grades.where(grader: self)
      comments = InlineComment.where(grade: grades).group_by(&:submission_id)
      self.assignment.course.students_with_drop_info.sort_by(&:sort_name).each do |s|
        if !s.dropped_date.nil?
          csv << [s.nuid, s.last_name, s.first_name, "Dropped"]
        else
          sub = subs[s.id]
          if sub.nil?
            csv << [s.nuid, s.last_name, s.first_name, "Missing"]
          else
            csv << [s.nuid, s.last_name, s.first_name, "", "", *comments[sub.id].sort_by(&:line).map(&:weight)]
          end
        end
      end
    end
    out.to_s
  end
  def export_data_schema
    "exam_export_schema.html"
  end
  def import_data(who_grades, file)
    csv = CSV.parse(file.read, headers: true, converters: :numeric)
    expected_headers = ["NUID", "Last name", "First name", "Status", "Action",
                        *self.assignment.flattened_questions.map{|q| q["name"]},
                        "Curved"]
    if csv.headers != expected_headers
      return {message: nil, alert: "Invalid headers for CSV file: expected #{expected_headers} but got #{csv.headers}"}
    end
    students_with_grades = {}
    errors = []
    csv.each do |row|
      next if row.length == 0
      row_hash = row.to_hash
      if row_hash["NUID"].nil?
        errors << "Cannot handle grades for #{row_hash["First name"]} #{row_hash["Last name"]} without an NUID"
      else
        if row_hash["Action"] == "Update"
          # Column 5 is first column of grades (counting from 0)
          students_with_grades[row_hash["NUID"]] = row.to_a.slice(5..-1).map(&:second)
        elsif row_hash["Action"] == "Delete"
          @sub = Submission.find_by(assignment: self.assignment, user: User.find_by(nuid: row_hash["NUID"]))
          @sub&.destroy!
        end
      end
    end
    ans = apply_all_exam_grades(who_grades, students_with_grades, :nuid)
    ans[:errors].push(*errors)
    {notice:
       if ans[:created] > 0 || ans[:updated] > 0
         "Created #{ans[:created]} #{"new grade".pluralize(ans[:created])}, and updated #{ans[:updated]} #{"existing grade".pluralize(ans[:updated])}"
       else
         nil
       end,
     errors:
       if ans[:errors].empty?
         nil
       else
         ("<p>#{"Problem".pluralize(ans[:errors].length)} updating #{"grade".pluralize(ans[:errors].length)}:</p>" +
          "<ul>" + ans[:errors].map{|msg| "<li>#{msg}</li>"}.join("\n") + "</ul>").html_safe
       end
    }
  end
  def import_data_schema
    "exam_import_schema.html"
  end

  def apply_all_exam_grades(who_grades, students_with_grades, key)
    @student_info = self.assignment.course.students.select(:username, :last_name, :first_name, :nickname, :id, :nuid)
                      .where(key => students_with_grades.keys)
    ans = {created: 0, updated: 0, errors: []}
    if @student_info.length != students_with_grades.length
      missing = students_with_grades.except(*@student_info.map(&key)).keys
      ans[:errors] << "Could not find #{"student".pluralize(missing.length)} with the following #{key.to_s.upcase.pluralize(missing.length)}: #{missing.to_sentence}"
    end
    # @used_subs = @assignment.used_submissions
    # @grade_comments = InlineComment.where(submission_id: @used_subs.map(&:id)).group_by(&:user_id)

    flattened = self.assignment.flattened_questions
    @student_info.each do |student|
      grades = students_with_grades[student[key]]
      if (grades.nil?)
        ans[:errors] << "Could not find grades for student with #{key.upcase} #{student[key]}"
        next
      elsif (grades.index(nil) || flattened.length) < (flattened.length - 1)
        ans[:errors] << "Grades for student #{student[key]} (#{student.display_name}) contained nil values"
        next
      end
      @sub = self.assignment.used_sub_for(student)
      if @sub.nil?
        @sub = Submission.create!(assignment: self.assignment,
                                  user: student,
                                  type: "ExamSub",
                                  created_at: self.assignment.due_date - 1.minute)
        ans[:created] += 1
      else
        ans[:updated] += 1
      end
      @sub.set_used_sub!
      @grade = Grade.find_or_create_by(grader_id: self.id, submission_id: @sub.id)
      if @grade.new_record?
        @grade.out_of = self.avail_score
      end
      grades.each_with_index do |g, q_num|
        @sub.grade_question!(who_grades, q_num, g)
      end
      expect_num_questions(flattened.count)
      grade(@assignment, @sub)
    end
    return ans
  end

  protected

  def do_grading(assignment, sub)
    g = self.grade_for sub
    comments = InlineComment.where(submission: sub, grade: g, suppressed: false).where('line < ?', @num_questions)
    score = comments.pluck(:weight).sum

    g.out_of = self.avail_score
    g.score = [0, score].max # can get extra credit above max score

    curved = InlineComment.find_by(submission: sub, grade: g, suppressed: false, line: @num_questions)
    if curved
      g.score = curved.weight
    end

    g.updated_at = DateTime.now
    g.available = false
    g.save!

    if g.score != sub.score
      sub.update(score: nil) # wipe out score until it's republished
    end

    return g.score
  end
end
