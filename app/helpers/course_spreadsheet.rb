require 'gradesheet'
require 'audit'
require 'spreadsheet'

class CourseSpreadsheet < Spreadsheet
  def initialize(course)
    @sheets = []

    exams, exam_cols = create_exams(course, Sheet.new("Exams"))
    hws, hw_cols = create_hws(course, Sheet.new("Homework"), exams)
    summary = create_summary(course, Sheet.new("Summary"), exams, exam_cols, hws, hw_cols)
    @sheets.push(summary, exams, hws)
    @sheets.each do |s|
      s.assign_coords
    end
  end

  def sanity_check
    @sheets.each do |s|
      s.sanity_check
    end
  end

  def create_exams(course, sheet)
    labels, weight, users = create_name_columns(course, sheet)

    exam_cols = []

    course.assignments.where(type: ["Exam", "Questions"]).order(:due_date).each do |exam|
      used_subs = exam.all_used_subs.to_a
      grades = Gradesheet.new(exam, used_subs)
      subs_for_grading = UsedSub.where(assignment: exam).to_a

      sheet.columns.push(Col.new(exam.name, "Number"))
      questions = exam.flattened_questions
      questions.each_with_index do |q, i|
        sheet.columns.push(Col.new("", "Number")) if i > 0
        if q["name"]
          labels.push(Cell.new(q["name"] + (if q["extra"] then " (E.C.)" else "" end)))
        else
          labels.push(Cell.new("Question #{i + 1}"))
        end
        weight.push(Cell.new(q["weight"]))
      end
      sheet.columns.push(Col.new("", "Number"), Col.new("", "Percent"), Col.new("", "Percent"), Col.new("", "Percent"))
      labels.push(Cell.new("Total"), Cell.new("Computed%"), Cell.new("Curved"), Cell.new("OnServer%"))
      normal_questions_and_indices = questions.zip(0..(questions.count - 1)).delete_if do |q, idx| q["extra"] end
      tot_weight = normal_questions_and_indices.map{|q, idx| q["weight"]}.sum()
      weight.push(Cell.new(nil, Formula.new(tot_weight, "SUM",
                                            *(normal_questions_and_indices.map do |q, idx|
                                                CellRef.new(nil,
                                                            Spreadsheet.col_name(weight.count - questions.count + idx), true,
                                                            3, true,
                                                            q["weight"])
                                              end))),
                  Cell.new(""), Cell.new(""), Cell.new(""))
      exam_cols.push [exam, weight.count - 2]

      all_grade_comments = InlineComment.where(submission_id: subs_for_grading.map(&:submission_id))
                           .group_by(&:submission_id)
      subs_by_user = subs_for_grading.map{|sfg| [sfg.user_id, sfg]}.to_h

      users.each_with_index do |u, i|
        sub_id = subs_by_user[u.id]
        begin
          sub = grades.grades[:grades][sub_id.submission_id] unless sub_id.nil?
        rescue Exception => e
          sub = nil
          Audit.log("Failed in query: #{grades.grades} and #{sub_id and sub_id.submission_id}\n#{e}\n")
        end
        if sub.nil?
          questions.each do |g| sheet.push_row(i, "") end
          sheet.push_row(i, [0, "No submission"])
          curved = Cell.new(nil,
                            CellRef.new(nil,
                                        Spreadsheet.col_name(weight.count - 3), true, i + sheet.header_rows.length + 2, false,
                                        nil))
          sheet.push_row(i, [curved, 0])
        elsif all_grade_comments[sub_id.submission_id].nil?
          questions.each do |g| sheet.push_row(i, "") end
          sheet.push_row(i, [0, "Not yet graded"])
          curved = Cell.new(nil,
                            CellRef.new(nil,
                                        Spreadsheet.col_name(weight.count - 3), true, i + sheet.header_rows.length + 2, false,
                                        nil))
          sheet.push_row(i, [curved, 0])
        else
          grade_comments = all_grade_comments[sub_id.submission_id].sort_by{|c| c.line}
          q_grades = grade_comments.slice(0..questions.count-1).map{|c| (c && c["weight"])}
          q_grades.each do |g|
            sheet.push_row(i, g || "<none>")
          end
          sum_grade = Formula.new(nil, "SUM",
                                  Range.new(Spreadsheet.col_name(weight.count - questions.count - 4), true,
                                            i + sheet.header_rows.length + 2, false,
                                            Spreadsheet.col_name(weight.count - 5), true,
                                            i + sheet.header_rows.length + 2, false))
          sheet.push_row(i, Cell.new(nil, sum_grade))
          sum_grade = Formula.new(sub[:sub].score, "/",
                                  sum_grade, CellRef.new(nil, Spreadsheet.col_name(weight.count - 4), true, 3, true, tot_weight))

          sheet.push_row(i, Cell.new(nil, sum_grade))
          curve = grade_comments[questions.count]
          if curve
            curved = Cell.new(nil, Formula.new(sub[:sub].score, "/", curve["weight"], tot_weight))
          else
            curved = Cell.new(nil,
                              CellRef.new(nil,
                                          Spreadsheet.col_name(weight.count - 3), true, i + sheet.header_rows.length + 2, false,
                                          nil))
          end
          sheet.push_row(i, curved)
          if sub[:sub].score
            sheet.push_row(i, sub[:sub].score / 100.0)
          else
            sheet.push_row(i, 0)
          end
        end
      end
    end
    
    return sheet, exam_cols
  end

  def create_name_columns(course, sheet)
    sheet.columns.push(
      Col.new("LastName", "String"), Col.new("FirstName", "String"), Col.new("Instructor", "String"),
      Col.new("NUID", "String"), Col.new("Username", "String"), Col.new("Email", "String"))
    sections = course.sections
    sections_by_type = sections.group_by(&:type)
    sections_by_crn = sections.map{|s| [s.crn, s]}.to_h
    course_section_types = sections_by_type.keys
    course_section_types.each do |type|
      sheet.columns.push(Col.new("#{type.humanize} section", "Number"))
    end
    sheet.columns.push(Col.new("Withdrawn?", "DateTime"), Col.new(""), Col.new(""))
    labels = sheet.push_header_row(nil, ["", "", "", "", "", "", "", "", ""].push(*course_section_types.count.times.map{|| ""}))
    weight = sheet.push_header_row(nil, ["", "", "", "", "", "", "", "", ""].push(*course_section_types.count.times.map{|| ""}))

    users = course.students.order(:last_name, :first_name).to_a

    regs = course.registrations.joins(:sections)
           .select(:user_id, :crn, :type, :dropped_date)
           .group_by(&:user_id)
           .map do |uid, sections|
      [uid, sections.map{|s| [s.type, {section: sections_by_crn[s.crn], dropped_date: s.dropped_date}]}.to_h]
    end.to_h
    users.each do |u|
      reg = regs[u.id]
      lecture = reg[Section::types["lecture"]]
      row = [ sanitize(u.last_name || u.name),
              sanitize(u.first_name || ""),
              sanitize(lecture&.dig(:section)&.instructor&.last_name || ""),
              u.nuid || "",
              sanitize(u.username),
              sanitize(u.email)]
      course_section_types.each do |type|
        section = reg[Section::types[type]]&.fetch(:section)
        row.push(section&.crn || "")
      end
      row.push(lecture&.dig(:dropped_date) || "", "", "")
      sheet.push_row(nil, row)
    end

    return labels, weight, users
  end
  
  def create_hws(course, sheet, exams)
    labels, weight, users = create_name_columns(course, sheet)

    hw_cols = []

    course.assignments.where.not(type: ["Exam", "Questions"]).order(:due_date).each do |assn|
      used_subs = assn.all_used_subs.includes(:user, :team).references(:user, :team).to_a
      grades = Gradesheet.new(assn, used_subs)
      subs_for_grading = UsedSub.where(assignment: assn).to_a
      assn.cache_effective_due_dates!(users)
      
      sheet.columns.push(Col.new((assn.extra_credit ? "(E.C.) " : "") + sanitize(assn.name), "Number"))
      @graders_count = grades.graders.count
      grades.graders.each_with_index do |g, i|
        sheet.columns.push(Col.new("", "Number")) if i > 0
        labels.push(Cell.new((g.extra_credit ? "(E.C.) " : "") + g.display_type))
        weight.push(Cell.new(g.avail_score))
      end
      sheet.columns.push(Col.new("", "Number"), Col.new("", "Percent"), Col.new("", "Percent"), Col.new("", "Percent"))
      labels.push(Cell.new("Total"), Cell.new("Lateness"), Cell.new("Computed%"), Cell.new("OnServer%"))
      graders = grades.graders.to_a
      tot_weight = graders.reject(&:extra_credit).map(&:avail_score).sum()
      grader_weights = graders.zip(0..graders.count).reject{|g, _| g.extra_credit}.map do |g, i|
        CellRef.new(nil, Spreadsheet.col_name(weight.count - @graders_count + i), true, 3, true, g.avail_score)
      end
      weight.push(Cell.new(nil, Formula.new(tot_weight, "SUM", *grader_weights)),
                  Cell.new(""), Cell.new(""), Cell.new(""))
      hw_cols.push [assn, weight.count - 2]

      subs_by_user = subs_for_grading.map{|sfg| [sfg.user_id, sfg]}.to_h

      users.each_with_index do |u, i|
        sub_id = subs_by_user[u.id]
        sub = grades.grades[:grades][sub_id.submission_id] unless sub_id.nil?
        if sub.nil?
          grades.graders.each do |g| sheet.push_row(i, "") end
          sheet.push_row(i, [0, "No submission", 0, 0])
        else
          sub[:staff_scores][:scores].each do |ss|
            sheet.push_row(i, ss[0] || "<none>")
          end
          sum_grade = Formula.new(sub[:sub].score, "SUM",
                                  Range.new(Spreadsheet.col_name(weight.count - @graders_count - 4), true,
                                            i + sheet.header_rows.length + 2, false,
                                            Spreadsheet.col_name(weight.count - 5), true,
                                            i + sheet.header_rows.length + 2, false))
          #sheet.push_row(i, sub[:staff_scores][:raw_score])
          sheet.push_row(i, Cell.new(nil, sum_grade))

          sum_grade = Formula.new(sub[:sub].score, "/",
                                  CellRef.new(nil,
                                              Spreadsheet.col_name(weight.count - 4), true,
                                              i + sheet.header_rows.length + 2, false,
                                              nil),
                                  CellRef.new(nil, Spreadsheet.col_name(weight.count - 4), true, 3, true, nil))

          if sub[:sub].ignore_late_penalty
            sheet.push_row(i, "<ignore>")
          else
            lc = assn.lateness_config
            penalty = lc.late_penalty(assn, sub[:sub]) / 100.0
            sheet.push_row(i, penalty)
            sum_grade = Formula.new(nil, "MAX", 0,
                                    Formula.new(nil, "-", sum_grade,
                                                CellRef.new(nil,
                                                            Spreadsheet.col_name(weight.count - 3), true,
                                                            i + sheet.header_rows.length + 2, false,
                                                            penalty)
                                               ))
          end


          sheet.push_row(i, Cell.new(nil, sum_grade))
          if sub[:sub].score
            sheet.push_row(i, sub[:sub].score / 100.0)
          else
            sheet.push_row(i, 0)
          end
        end
      end
    end
    
    return sheet, hw_cols
  end

  def create_summary(course, sheet, exams, exam_cols, hws, hw_cols)
    labels, weight, users = create_name_columns(course, sheet)

    hw_headers = hws.header_rows.count + 1 + 1 # 1 for the header labels, and one because 1-indexed

    start_col = sheet.columns.count

    hw_cols.each do |assn, col|
      sheet.columns.push(Col.new((assn.extra_credit ? "(E.C.) " : "") + sanitize(assn.name), "Percent"))
      labels.push(Cell.new(assn.points_available / 100.0))
      weight.push(Cell.new(""))

      users.each_with_index do |u, i|
        sheet.push_row(i, Cell.new(nil, CellRef.new(hws.name, Spreadsheet.col_name(col), true, i + hw_headers, false,
                                                    "Please Recalculate")))
      end
    end

    exam_cols.each do |exam, col|
      sheet.columns.push(Col.new(exam.name, "Percent"))
      labels.push(Cell.new(exam.points_available / 100.0))
      weight.push(Cell.new(""))

      users.each_with_index do |u, i|
        sheet.push_row(i, Cell.new(nil, CellRef.new(exams.name, Spreadsheet.col_name(col), true, i + hw_headers, false,
                                                    "Please Recalculate")))
      end
    end

    end_col = sheet.columns.count - 1

    sheet.columns.push(Col.new("Total", "Percent"))
    labels.push(Cell.new(""))
    weight.push(Cell.new(""))

    users.each_with_index do |u, i|
      sheet.push_row(i, Cell.new(nil, Formula.new("Please recalculate", "SUMPRODUCT",
                                                  Range.new(Spreadsheet.col_name(start_col), true, 2, true,
                                                            Spreadsheet.col_name(end_col), true, 2, true),
                                                  Range.new(Spreadsheet.col_name(start_col), true, i + hw_headers, false,
                                                            Spreadsheet.col_name(end_col), true, i + hw_headers, false))))
    end

    sheet
  end
end
