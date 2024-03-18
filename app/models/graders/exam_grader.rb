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
    "exam_export_schema"
  end
  def import_data(who_grades, file)
    contents = file.read
    if contents.starts_with?("\xEF\xBB\xBF")
      contents = contents.sub!("\xEF\xBB\xBF", '')
    end
    csv = CSV.parse(contents, headers: true, converters: :numeric)
    expected_headers = ["NUID", "Last name", "First name", "Status", "Action",
                        *self.assignment.flattened_questions.map{|q| q["name"]},
                        "Curved"]
    if csv.headers != expected_headers
      return {message: nil, errors: "Invalid headers for CSV file: expected #{expected_headers} but got #{csv.headers}"}
    end
    students_with_grades = {}
    csv_by_nuid = csv.filter{|r| r.length > 0}.group_by{|r| r["NUID"].nil?}
    errors = csv_by_nuid[true]&.map do |r|
      "Cannot handle grades for #{r["First name"]} #{r["Last name"]} without an NUID"
    end || []
    csv_by_action = (csv_by_nuid[false] || []).group_by{|r| r["Action"]}
    nuids = (csv_by_nuid[false] || []).map { |row| row["NUID"] }
    users_by_nuid = User.where(nuid: nuids).to_h { |u| [u.nuid, u] }
    subs_by_user = Submission.where(assignment: self.assignment, user: users_by_nuid.values)
                     .to_h { |s| [s.user_id, s] }
    students_with_grades = (csv_by_action["Update"] || []).to_h do |r|
      [r["NUID"], r.fields.slice(5..-1)]
    end
    @subs_to_destroy = (csv_by_action["Delete"] || []).map{|r| subs_by_user[users_by_nuid[r["NUID"]]&.id]}.compact
    Submission.bulk_delete(@subs_to_destroy.map(&:id))
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
    "exam_import_schema"
  end

  def apply_all_exam_grades(who_grades, students_with_grades, key)
    @student_info = self.assignment.course.students
                      .select(:username, :last_name, :first_name, :nickname, :id, :nuid)
                      .where(key => students_with_grades.keys)
    ans = {created: 0, updated: 0, errors: []}
    if @student_info.length != students_with_grades.length
      missing = students_with_grades.except(*@student_info.map(&key)).keys
      ans[:errors] << "Could not find #{"student".pluralize(missing.length)} with the following #{key.to_s.upcase.pluralize(missing.length)}: #{missing.to_sentence}"
    end
    # @used_subs = @assignment.used_submissions
    # @grade_comments = InlineComment.where(submission_id: @used_subs.map(&:id)).group_by(&:user_id)

    flattened = self.assignment.flattened_questions
    @grades_by_sub = self.grades.includes(:submission).to_h {|g| [g.submission_id, g]}
    InlineComment.transaction do
      @used_submissions = self.assignment.all_used_subs
                              .includes(:assignment, :inline_comments, :grades, :team, :user, :user_submissions)
                              .to_h {|s| [s.user_id, s]}
      @comments_to_insert = []
      @student_info.each do |student|
        grades = students_with_grades[student[key]]
        if (grades.nil?)
          ans[:errors] << "Could not find grades for student with #{key.upcase} #{student[key]}"
          next
        elsif (grades.index(nil) || flattened.length) < (flattened.length - 1)
          ans[:errors] << "Grades for student #{student[key]} (#{student.display_name}) contained nil values"
          next
        end
        @sub = @used_submissions[student.id]
        if @sub.nil?
          @sub = Submission.create!(assignment: self.assignment,
                                    user: student,
                                    type: "ExamSub",
                                    created_at: self.assignment.due_date - 1.minute)
          @sub.score = compute_score(grades, flattened.count, percent: true)
          UsedSub.create!(user: student, assignment: self.assignment, submission: @sub)
          @used_submissions[student.id] = @sub
          ans[:created] += 1
        else
          @sub.update(score: compute_score(grades, flattened.count))
          ans[:updated] += 1
        end
        @sub.cache_grading_comments!
        @grade = @grades_by_sub[@sub.id] ||= Grade.new(grader_id: self.id, submission_id: @sub.id)
        if @grade.new_record?
          @grade.out_of = self.avail_score
        end
        @grade.score = compute_score(grades, flattened.count)
        @grade.available = false
        @grade.save!
        grades.each_with_index do |g, q_num|
          @comments_to_insert << @sub.grading_comment_attributes(who_grades, @grade, q_num, g)
        end
        #do_raw_grading(@sub, @grade, grades, flattened.count)
      end
      @comments_to_insert.compact!
      InlineComment.upsert_all(@comments_to_insert) unless @comments_to_insert.blank?
      @student_info.each do |student|
        @sub = @used_submissions[student.id]
        @grade = @grades_by_sub[@sub.id]
        expect_num_questions(flattened.count)
      end
    end
    return ans
  end

  protected
  def compute_score(grades, num_questions, percent: false)
    score = grades[num_questions].blank? ? grades.compact.sum : grades[num_questions]
    score = [0, score].max
    if percent
      100.0 * score / self.avail_score
    else
      score
    end
  end
  def do_raw_grading(sub, grade, grades, num_questions)
    grade.out_of = self.avail_score
    score = grades[num_questions].blank? ? grades.compact.sum : grades[num_questions]
    grade.score = [0, score].max
    sub.score = 100.0 * grade.score / grade.out_of
    grade.updated_at = DateTime.now
    grade.available = false
  end
  
  def do_grading(assignment, sub, grade = nil)
    g = grade || self.grade_for(sub)
    comments = sub.inline_comments.filter{|c| c.grade_id == g.id && !c.suppressed && c.line < @num_questions}
    score = comments.pluck(:weight).sum

    g.out_of = self.avail_score
    g.score = [0, score].max # can get extra credit above max score

    curved = sub.inline_comments.find {|c| c.grade_id == g.id && !c.suppressed && c.line == @num_questions}
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
