require 'gradesheet'
require 'audit'
require 'spreadsheet'

class CourseSpreadsheet < Spreadsheet
  def initialize(course)
    @sheets = []
    @course = course
    @users = @course.students.order(:last_name, :first_name).to_a
    @users_by_id = @users.map {|u| [u.id, u]}.to_h
    @sections = @course.sections
    @sections_by_type = @sections.group_by(&:type)
    @sections_by_crn = @sections.map{|s| [s.crn, s]}.to_h

    @regs = @course.registrations.joins(:sections)
           .select(:user_id, :crn, :type, :dropped_date, :created_at)
           .group_by(&:user_id)
           .map do |uid, sections|
      [uid, sections.map{|s| [s.type, {section: @sections_by_crn[s.crn], joined_date: s.created_at, dropped_date: s.dropped_date}]}.to_h]
    end.to_h

    exams, exam_cols = create_exams(Sheet.new("Exams"))
    hws, hw_cols = create_hws(Sheet.new("Homework"), exams)
    summary = create_summary(Sheet.new("Summary"), exams, exam_cols, hws, hw_cols)
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

  def create_exams(sheet)
    labels, weight = create_name_columns(sheet)
    sheet.freeze_panes(sheet.header_rows.length + 1, labels.length - 2)

    exam_cols = []

    # Skip two rows after all the user rows, to make room for stats-per-section rows
    sheet.push_row(nil, [])
    sheet.push_row(nil, [])

    stats_per_section = {}
    stats_rows = []
      

    @course.assignments.where(type: ["Exam", "Questions"]).order(:due_date).each do |exam|
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

      @users.each_with_index do |u, i|
        sub_id = subs_by_user[u.id]
        begin
          sub = grades.grades[:grades][sub_id.submission_id] unless sub_id.nil?
        rescue Exception => e
          sub = nil
          Audit.log("Failed in query: #{grades.grades} and #{sub_id and sub_id.submission_id}\n#{e}")
        end
        if sub.nil?
          questions.each do |g| sheet.push_row(i, "") end
          sheet.push_row(i, ["No submission", "No submission"])
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
          if exam.is_a? Exam
            # Exam scores are absolute scores, so just sum them up
            q_grades = grade_comments.slice(0..questions.count-1).map{|c| (c && c["weight"])}
          else
            # Questions grades are percentages (like codereviews), so sum must be weighted first
            q_grades = grade_comments.slice(0..questions.count-1).zip(questions)
                       .map{|c, q| c && (c["weight"] * q["weight"])}
          end
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
            sheet.push_row(i, sub[:sub].score.to_f / 100.0)
          else
            sheet.push_row(i, 0)
          end
        end
      end

      first_question_col = weight.count - questions.count - 4
      @sections_by_type.each_with_index do |(type, secs), sec_type_idx|
        if stats_per_section[type].nil?
          stats_per_section[type] =
            [""] * (first_question_col - 2) + ["#{type.humanize} #{"section".pluralize(secs.count)}"]
          stats_rows.push stats_per_section[type]
        end
        secs.each do |sec|
          if stats_per_section[sec.crn].nil?
            stats_per_section[sec.crn] = {
              min: [""] * (first_question_col - 2),
              avg: [""] * (first_question_col - 2),
              std: [""] * (first_question_col - 2),
              max: [""] * (first_question_col - 2),
            }
            stats_rows.push stats_per_section[sec.crn][:min]
            stats_rows.push stats_per_section[sec.crn][:avg]
            stats_rows.push stats_per_section[sec.crn][:std]
            stats_rows.push stats_per_section[sec.crn][:max]
          end
          min_row = stats_per_section[sec.crn][:min]
          avg_row = stats_per_section[sec.crn][:avg]
          std_row = stats_per_section[sec.crn][:std]
          max_row = stats_per_section[sec.crn][:max]
          
          min_row.push(*(([""] * (first_question_col - min_row.count - 2)) + ["#{sec.to_s(show_type: false)}", "min"]))
          avg_row.push(*(([""] * (first_question_col - avg_row.count - 2)) + ["", "avg"]))
          std_row.push(*(([""] * (first_question_col - std_row.count - 2)) + ["", "stdev"]))
          max_row.push(*(([""] * (first_question_col - max_row.count - 2)) + ["", "max"]))

          (0 .. questions.count).each do |idx|
            question_values = Range.new(Spreadsheet.col_name(first_question_col + idx), true,
                                        sheet.header_rows.length + 2, true,
                                        Spreadsheet.col_name(first_question_col + idx), true,
                                        @users.count + sheet.header_rows.length + 1, true)
            sections_values = Range.new(Spreadsheet.col_name(6 + sec_type_idx), true,
                                        sheet.header_rows.length + 2, true,
                                        Spreadsheet.col_name(6 + sec_type_idx), true,
                                        @users.count + sheet.header_rows.length + 1, true)

            min_row << Cell.new(nil, Formula.new(nil, "MIN",
                                                 Formula.new(nil, "IF",
                                                             Formula.new(nil, "=", sections_values, sec.crn),
                                                             question_values, "\"\"")), true)
            avg_row << Cell.new(nil, Formula.new(nil, "AVERAGE",
                                                 Formula.new(nil, "IF",
                                                             Formula.new(nil, "=", sections_values, sec.crn),
                                                             question_values, "\"\"")), true)
            std_row << Cell.new(nil, Formula.new(nil, "STDEVP",
                                                 Formula.new(nil, "IF",
                                                             Formula.new(nil, "=", sections_values, sec.crn),
                                                             question_values, "\"\"")), true)
            max_row << Cell.new(nil, Formula.new(nil, "MAX",
                                                 Formula.new(nil, "IF",
                                                             Formula.new(nil, "=", sections_values, sec.crn),
                                                             question_values, "\"\"")), true)
          end
        end
      end
      if stats_per_section["ALL"].nil?
        stats_per_section["ALL"] = {
          min: [""] * (first_question_col - 2),
          avg: [""] * (first_question_col - 2),
          std: [""] * (first_question_col - 2),
          max: [""] * (first_question_col - 2),
        }
        stats_rows.push []
        stats_rows.push stats_per_section["ALL"][:min]
        stats_rows.push stats_per_section["ALL"][:avg]
        stats_rows.push stats_per_section["ALL"][:std]
        stats_rows.push stats_per_section["ALL"][:max]
      end
      min_row = stats_per_section["ALL"][:min]
      avg_row = stats_per_section["ALL"][:avg]
      std_row = stats_per_section["ALL"][:std]
      max_row = stats_per_section["ALL"][:max]

      min_row.push(*(([""] * (first_question_col - min_row.count - 2)) + ["ALL SECTIONS", "min"]))
      avg_row.push(*(([""] * (first_question_col - avg_row.count - 2)) + ["", "avg"]))
      std_row.push(*(([""] * (first_question_col - std_row.count - 2)) + ["", "stdev"]))
      max_row.push(*(([""] * (first_question_col - max_row.count - 2)) + ["", "max"]))

      (0 .. questions.count).each do |idx|
        question_values = Range.new(Spreadsheet.col_name(first_question_col + idx), true,
                                    sheet.header_rows.length + 2, true,
                                    Spreadsheet.col_name(first_question_col + idx), true,
                                    @users.count + sheet.header_rows.length + 1, true)
        min_row << Cell.new(nil, Formula.new(nil, "MIN", question_values))
        avg_row << Cell.new(nil, Formula.new(nil, "AVERAGE", question_values))
        std_row << Cell.new(nil, Formula.new(nil, "STDEVP", question_values))
        max_row << Cell.new(nil, Formula.new(nil, "MAX", question_values))
      end
    end

    stats_rows.each do |r| sheet.push_row(nil, r) end
    
    return sheet, exam_cols
  end

  def create_name_columns(sheet)
    sheet.columns.push(
      Col.new("LastName", "String"), Col.new("FirstName", "String"), Col.new("Instructor", "String"),
      Col.new("NUID", "String"), Col.new("Username", "String"), Col.new("Email", "String"))
    course_section_types = @sections_by_type.keys
    course_section_types.each do |type|
      sheet.columns.push(Col.new("#{type.humanize} section", "Number"))
    end
    sheet.columns.push(Col.new("Withdrawn?", "DateTime"), Col.new(""), Col.new(""))
    labels = sheet.push_header_row(nil, ["", "", "", "", "", "", "", "", ""].push(*course_section_types.count.times.map{|| ""}))
    weight = sheet.push_header_row(nil, ["", "", "", "", "", "", "", "", ""].push(*course_section_types.count.times.map{|| ""}))

    @users.each do |u|
      reg = @regs[u.id]
      lecture = reg[Section::types["lecture"]] || reg[Section::types["online"]]
      row = [ sanitize(u.last_name || u.name || "<anonymous>"),
              sanitize(u.first_name || ""),
              sanitize(lecture&.dig(:section)&.instructor&.last_name || "<no instructor>"),
              u.nuid || "<###>",
              sanitize(u.username || "<no username>"),
              sanitize(u.email || "<no email>")]
      course_section_types.each do |type|
        section = reg[Section::types[type]]&.fetch(:section)
        row.push(section&.crn || "<no CRN>")
      end
      row.push(lecture&.dig(:dropped_date) || "", "", "")
      sheet.push_row(nil, row)
    end

    return labels, weight
  end
  
  def create_hws(sheet, exams)
    labels, weight  = create_name_columns(sheet)
    sheet.freeze_panes(sheet.header_rows.length + 1, labels.length - 2)

    hw_cols = []

    # Skip two rows after all the user rows, to make room for stats-per-section rows
    sheet.push_row(nil, [])
    sheet.push_row(nil, [])

    stats_per_section = {}
    stats_rows = []
      

    @course.assignments.where.not(type: ["Exam", "Questions"]).order(:due_date).each do |assn|
      used_subs = assn.all_used_subs.includes(:user, :team).references(:user, :team).to_a
      grades = Gradesheet.new(assn, used_subs)
      subs_for_grading = UsedSub.where(assignment: assn).to_a
      assn.cache_effective_due_dates!(@users)
      
      sheet.columns.push(Col.new((assn.extra_credit ? "(E.C.) " : "") + sanitize(assn.name || "Assignment #{assn.id}"), "Number"))
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

      @users.each_with_index do |u, i|
        sub_id = subs_by_user[u.id]
        sub = grades.grades[:grades][sub_id.submission_id] unless sub_id.nil?
        if sub.nil?
          grades.graders.each do |g| sheet.push_row(i, "") end
          user_regs = @regs[u.id]
          joined_date = user_regs.values.map{|r| r[:joined_date]}.min
          dropped_date = user_regs.values.map{|r| r[:dropped_date]}.min
          if joined_date && assn.due_date <= joined_date
            sheet.push_row(i, ["Late registration", "Late registration", "Late registration", "Late registration"])
          elsif dropped_date && assn.due_date >= dropped_date
            sheet.push_row(i, ["Dropped", "Dropped", "Dropped", "Dropped"])
          else
            sheet.push_row(i, ["No submission", "No submission", 0, 0])
          end
        else
          plagiarism_status = grades.plagiarism_status[sub_id.submission_id]
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
          elsif plagiarism_status
            penalty = plagiarism_status.pluck(:weight).sum
            sheet.push_row(i, "Plagiarized (-#{penalty} pts)")
            # (SumGrade - PlagiarismPenalty) / MaxPoints
            sum_grade = Formula.new(nil, "MAX", 0,
                                    Formula.new(sub[:sub].score, "/",
                                                Formula.new(nil, "-",
                                                            CellRef.new(nil,
                                                                        Spreadsheet.col_name(weight.count - 4), true,
                                                                        i + sheet.header_rows.length + 2, false,
                                                                        nil),
                                                            penalty),
                                                CellRef.new(nil,
                                                            Spreadsheet.col_name(weight.count - 4), true,
                                                            3, true,
                                                            nil)))
          else
            lc = assn.lateness_config
            penalty = lc.late_penalty(assn, sub[:sub]) / 100.0
            sheet.push_row(i, penalty)
            # (SumGrade / PlagiarismPenalty) - LatenessPercent
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
            sheet.push_row(i, sub[:sub].score.to_f / 100.0)
          else
            sheet.push_row(i, 0)
          end
        end
      end

      first_grader_col = weight.count - graders.count - 4
      @sections_by_type.each_with_index do |(type, secs), sec_type_idx|
        if stats_per_section[type].nil?
          stats_per_section[type] =
            [""] * (first_grader_col - 2) + ["#{type.humanize} #{"section".pluralize(secs.count)}"]
          stats_rows.push stats_per_section[type]
        end
        secs.each do |sec|
          if stats_per_section[sec.crn].nil?
            stats_per_section[sec.crn] = {
              min: [""] * (first_grader_col - 2),
              avg: [""] * (first_grader_col - 2),
              std: [""] * (first_grader_col - 2),
              max: [""] * (first_grader_col - 2),
            }
            stats_rows.push stats_per_section[sec.crn][:min]
            stats_rows.push stats_per_section[sec.crn][:avg]
            stats_rows.push stats_per_section[sec.crn][:std]
            stats_rows.push stats_per_section[sec.crn][:max]
          end
          min_row = stats_per_section[sec.crn][:min]
          avg_row = stats_per_section[sec.crn][:avg]
          std_row = stats_per_section[sec.crn][:std]
          max_row = stats_per_section[sec.crn][:max]
          
          min_row.push(*(([""] * (first_grader_col - min_row.count - 2)) + ["#{sec.to_s(show_type: false)}", "min"]))
          avg_row.push(*(([""] * (first_grader_col - avg_row.count - 2)) + ["", "avg"]))
          std_row.push(*(([""] * (first_grader_col - std_row.count - 2)) + ["", "stdev"]))
          max_row.push(*(([""] * (first_grader_col - max_row.count - 2)) + ["", "max"]))

          (0 .. graders.count).each do |idx|
            grader_values = Range.new(Spreadsheet.col_name(first_grader_col + idx), true,
                                      sheet.header_rows.length + 2, true,
                                      Spreadsheet.col_name(first_grader_col + idx), true,
                                      @users.count + sheet.header_rows.length + 1, true)
            sections_values = Range.new(Spreadsheet.col_name(6 + sec_type_idx), true,
                                        sheet.header_rows.length + 2, true,
                                        Spreadsheet.col_name(6 + sec_type_idx), true,
                                        @users.count + sheet.header_rows.length + 1, true)

            min_row << Cell.new(nil, Formula.new(nil, "MIN",
                                                 Formula.new(nil, "IF",
                                                             Formula.new(nil, "=", sections_values, sec.crn),
                                                             grader_values, "\"\"")), true)
            avg_row << Cell.new(nil, Formula.new(nil, "AVERAGE",
                                                 Formula.new(nil, "IF",
                                                             Formula.new(nil, "=", sections_values, sec.crn),
                                                             grader_values, "\"\"")), true)
            std_row << Cell.new(nil, Formula.new(nil, "STDEVP",
                                                 Formula.new(nil, "IF",
                                                             Formula.new(nil, "=", sections_values, sec.crn),
                                                             grader_values, "\"\"")), true)
            max_row << Cell.new(nil, Formula.new(nil, "MAX",
                                                 Formula.new(nil, "IF",
                                                             Formula.new(nil, "=", sections_values, sec.crn),
                                                             grader_values, "\"\"")), true)
          end
        end
      end
      if stats_per_section["ALL"].nil?
        stats_per_section["ALL"] = {
          min: [""] * (first_grader_col - 2),
          avg: [""] * (first_grader_col - 2),
          std: [""] * (first_grader_col - 2),
          max: [""] * (first_grader_col - 2),
        }
        stats_rows.push []
        stats_rows.push stats_per_section["ALL"][:min]
        stats_rows.push stats_per_section["ALL"][:avg]
        stats_rows.push stats_per_section["ALL"][:std]
        stats_rows.push stats_per_section["ALL"][:max]
      end
      min_row = stats_per_section["ALL"][:min]
      avg_row = stats_per_section["ALL"][:avg]
      std_row = stats_per_section["ALL"][:std]
      max_row = stats_per_section["ALL"][:max]

      min_row.push(*(([""] * (first_grader_col - min_row.count - 2)) + ["ALL SECTIONS", "min"]))
      avg_row.push(*(([""] * (first_grader_col - avg_row.count - 2)) + ["", "avg"]))
      std_row.push(*(([""] * (first_grader_col - std_row.count - 2)) + ["", "stdev"]))
      max_row.push(*(([""] * (first_grader_col - max_row.count - 2)) + ["", "max"]))

      (0 .. graders.count).each do |idx|
        grader_values = Range.new(Spreadsheet.col_name(first_grader_col + idx), true,
                                    sheet.header_rows.length + 2, true,
                                    Spreadsheet.col_name(first_grader_col + idx), true,
                                    @users.count + sheet.header_rows.length + 1, true)
        min_row << Cell.new(nil, Formula.new(nil, "MIN", grader_values))
        avg_row << Cell.new(nil, Formula.new(nil, "AVERAGE", grader_values))
        std_row << Cell.new(nil, Formula.new(nil, "STDEVP", grader_values))
        max_row << Cell.new(nil, Formula.new(nil, "MAX", grader_values))
      end

    end

    stats_rows.each do |r| sheet.push_row(nil, r) end
    
    return sheet, hw_cols
  end

  def create_summary(sheet, exams, exam_cols, hws, hw_cols)
    labels, weight = create_name_columns(sheet)
    raw_label_count = labels.length - 3
    sheet.freeze_panes(sheet.header_rows.length + 1, raw_label_count + 1)

    hw_headers = hws.header_rows.count + 1 + 1 # 1 for the header labels, and one because 1-indexed

    start_col = sheet.columns.count

    hw_cols.each do |assn, col|
      sheet.columns.push(Col.new((assn.extra_credit ? "(E.C.) " : "") + sanitize(assn.name || "Assignment #{assn.id}"), "Percent"))
      labels.push(Cell.new(assn.points_available / 100.0))
      weight.push(Cell.new(""))

      @users.each_with_index do |u, i|
        sheet.push_row(i, Cell.new(nil, CellRef.new(hws.name, Spreadsheet.col_name(col), true, i + hw_headers, false,
                                                    "Please Recalculate")))
      end
    end

    exam_cols.each do |exam, col|
      sheet.columns.push(Col.new(exam.name, "Percent"))
      labels.push(Cell.new(exam.points_available / 100.0))
      weight.push(Cell.new(""))

      @users.each_with_index do |u, i|
        sheet.push_row(i, Cell.new(nil, CellRef.new(exams.name, Spreadsheet.col_name(col), true, i + hw_headers, false,
                                                    "Please Recalculate")))
      end
    end

    end_col = sheet.columns.count - 1

    sheet.columns.push(Col.new("Total", "Percent"))
    labels.push(Cell.new(""))
    weight.push(Cell.new(""))

    @users.each_with_index do |u, i|
      sheet.push_row(i, Cell.new(nil, Formula.new("Please recalculate", "SUMPRODUCT",
                                                  Range.new(Spreadsheet.col_name(start_col), true, 2, true,
                                                            Spreadsheet.col_name(end_col), true, 2, true),
                                                  Range.new(Spreadsheet.col_name(start_col), true, i + hw_headers, false,
                                                            Spreadsheet.col_name(end_col), true, i + hw_headers, false))))
    end

    ###############################
    # Create grade vlookup
    @grades = ["F", "D-", "D",  "D+", "C-", "C",  "C+", "B-", "B",  "B+", "A-", "A" ]
    @breaks = [0.0, 0.60, 0.63, 0.66, 0.70, 0.73, 0.76, 0.80, 0.83, 0.86, 0.90, 0.93]

    sheet.columns.push(Col.new("FINAL", "String"), Col.new("", "String"), Col.new("", "String"))
    sheet.columns.push(Col.new("Cutoff", "Percent"), Col.new("Grade", "String"), Col.new("Count", "Number"))
    @users.each_with_index do |u, i|
      sheet.push_row(i, Cell.new(nil, Formula.new("Please recalculate", "IF",
                                                  Formula.new("Please recalculate", "=",
                                                              CellRef.new(nil,
                                                                          Spreadsheet.col_name(raw_label_count), true,
                                                                          i + hw_headers, false,
                                                                          "FALSE"),
                                                              "\"\""),
                                                  Formula.new("Please recalculate", "VLOOKUP",
                                                              CellRef.new(nil, Spreadsheet.col_name(end_col + 1), true,
                                                                          i + hw_headers, false,
                                                                          "Please recalculate"),
                                                              Range.new(Spreadsheet.col_name(end_col + 5), true,
                                                                        hw_headers + 1, true,
                                                                        Spreadsheet.col_name(end_col + 6), true,
                                                                        hw_headers + @grades.count, true),
                                                              2),
                                                  "\"W\"")))
    end

    #################################
    # Create grade histogram
    sheet.pad_to_coords([@grades.length + 3, @breaks.length + 1].max, sheet.rows[0].size)
    sheet.push_row(0, ["", "", "", "W"])
    @breaks.zip(@grades).each_with_index do |(b, g), i|
      sheet.push_row(i + 1, ["", "", b, g])
    end
    (0..@grades.length).each do |i|
      sheet.push_row(i, Cell.new(nil, Formula.new("Please recalculate", "COUNTIF",
                                                  Range.new(Spreadsheet.col_name(end_col + 2), true,
                                                            hw_headers, true,
                                                            Spreadsheet.col_name(end_col + 2), true,
                                                            hw_headers + @users.count - 1, true),
                                                  CellRef.new(nil,
                                                              Spreadsheet.col_name(end_col + 6), true,
                                                              i + hw_headers, true,
                                                              @grades[i]))))
    end

    chart = Chart.new
    chart.add_series(nil,
                     Range.new(Spreadsheet.col_name(end_col + 6), true, hw_headers, true,
                               Spreadsheet.col_name(end_col + 6), true, hw_headers + @grades.count, true),
                     Range.new(Spreadsheet.col_name(end_col + 7), true, hw_headers, true,
                               Spreadsheet.col_name(end_col + 7), true, hw_headers + @grades.count, true))
    sheet.push_row(@grades.length + 2, ["", "", Cell.new(chart)])

    sheet
  end
end
