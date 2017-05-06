require 'gradesheet'
require 'write_xlsx'
require 'stringio'
require 'audit'


def col_index(name)
  @columns.find_index{|c| c.name == name}
end
def col_delta(from, to)
  col_index(from) - col_index(to)
end
def row_index(col_name, value)
  ci = col_index(col_name)
  @rows.find_index{|r| r[ci] == value} + @header_rows.length
end
def col_name(index)
  # zero-based, so col_name(0) == "A", col_name(26) == "AA"
  alph = ("A".."Z").to_a
  s, q = "", index + 1
  (q, r = (q - 1).divmod(26)) && s.prepend(alph[r]) until q.zero?
  s
end



class CourseSpreadsheet
  attr_reader :sheets

  class Col
    attr_reader :name
    attr_reader :type
    attr_accessor :col
    def initialize(name, type=nil)
      @name = name
      @type = type || "String"
    end
  end

  class Formula
    attr_accessor :function
    attr_accessor :args
    attr_accessor :value
    def initialize(value, function, *args)
      @value = value
      @function = function
      @args = args
    end
    def to_s
      if @function.length == 1 and @args.length == 2
        "(#{@args[0]} #{@function} #{@args[1]})"
      else
        "#{@function}(#{@args.map(&:to_s).join(',')})"
      end
    end
  end

  class CellRef
    attr_accessor :sheet_name
    attr_accessor :value
    attr_accessor :col
    attr_accessor :row
    attr_accessor :col_abs
    attr_accessor :row_abs
    def initialize(sheet_name, col, col_abs, row, row_abs, value)
      @sheet_name = sheet_name
      @col = col
      @row = row
      @col_abs = "$" if col_abs
      @row_abs = "$" if row_abs
    end
    def to_s
      ans = "#{@col_abs}#{@col}#{@row_abs}#{@row}"
      unless @sheet_name.nil?
        ans = "#{@sheet_name}!#{ans}"
      end
      ans
    end
  end

  class Range
    attr_accessor :from_col
    attr_accessor :from_row
    attr_accessor :to_col
    attr_accessor :to_row
    attr_accessor :from_col_abs
    attr_accessor :from_row_abs
    attr_accessor :to_col_abs
    attr_accessor :to_row_abs
    def initialize(from_col, from_col_abs, from_row, from_row_abs, to_col, to_col_abs, to_row, to_row_abs)
      @from_col = from_col
      @from_row = from_row
      @to_col = to_col
      @to_row = to_row
      @from_col_abs = "$" if from_col_abs
      @from_row_abs = "$" if from_row_abs
      @to_col_abs = "$" if to_col_abs
      @to_row_abs = "$" if to_row_abs
    end
    def to_s
      "#{@from_col_abs}#{@from_col}#{@from_row_abs}#{@from_row}:#{@to_col_abs}#{@to_col}#{@to_row_abs}#{@from_row}"
    end
    def contains(col, row)
      print "col: #{col}, row: #{row}, from_col/row: #{@from_col}/#{@from_row}, to_col/row: #{@to_col}/#{@to_row}\n"
      (@from_row <= row) and (row <= @to_row) and (@from_col <= col) and (col <= @to_col)
    end
  end
  
  class Cell
    attr_reader :value
    attr_reader :formula
    attr_accessor :sheet_name
    attr_accessor :row
    attr_accessor :col
    def initialize(value=nil, formula=nil)
      debugger if value.nil? and formula.nil?
      @value = value
      @formula = formula
    end

    def sanity_check
      return if @value
      return unless @row and @col
      if @formula.is_a? Formula
        @formula.args.each do |a|
          if a.is_a? Range and a.contains(@col, @row)
            raise RangeError, "Cell at #{@sheet_name}!#{@col}#{@row} contains formula #{@formula} that overlaps itself"
          end
        end
      elsif @formula.is_a? CellRef
        if @sheet_name == @formula.sheet_name and @row == @formula.row and @col == @formula.col
          raise RangeError, "CellRef at #{@formula} refers to itself"
        end
      end
    end
  end

  class Sheet
    attr_reader :name
    attr_reader :header_rows
    attr_reader :rows
    attr_reader :columns
    def initialize(name, columns=nil, header_rows=nil, rows=nil)
      @name = name
      @columns = columns || []
      @header_rows = header_rows || []
      @rows = rows || []
    end

    def push_row(i, values)
      push_into(@rows, i, values)
    end

    def push_header_row(i, values)
      push_into(@header_rows, i, values)
    end

    def assign_coords
      @columns.each_with_index do |cell, c|
        cell.col = col_name(c)
      end
      @header_rows.each_with_index do |row, r|
        row.each_with_index do |cell, c|
          cell.sheet_name = @name
          cell.row = r + 1 + 1 # 1 for header row, and 1 for 1-based indexing
          cell.col = col_name(c)
        end
      end
      @rows.each_with_index do |row, r|
        row.each_with_index do |cell, c|
          cell.sheet_name = @name
          cell.row = r + @header_rows.length + 1 + 1 # 1 for header row, and 1 for 1-based indexing
          cell.col = col_name(c)
        end
      end
    end

    def sanity_check
      @header_rows.each do |r|
        r.each do |c| c.sanity_check end
      end
      @rows.each do |r|
        r.each do |c| c.sanity_check end
      end
    end


    protected
    def to_cell(val)
      if val.instance_of?(Cell)
        val
      else
        Cell.new(val)
      end
    end

    def push_into(arr, i, vals)
      unless vals.instance_of?(Array)
        vals = [vals]
      end
      if i.nil?
        row = vals.map{|v| to_cell(v)}
        arr.push(row)
        row
      else
        vals.map{|v| arr[i].push(to_cell(v)) }
        arr[i]
      end
    end
  end


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

    course.assignments.where(type: "exam").order(:due_date).each do |exam|
      used_subs = exam.all_used_subs.to_a
      grades = Gradesheet.new(exam, used_subs)
      subs_for_grading = UsedSub.where(assignment: exam).to_a

      sheet.columns.push(Col.new(exam.name, "Number"))
      questions = exam.flattened_questions
      questions.each_with_index do |q, i|
        sheet.columns.push(Col.new("", "Number")) if i > 0
        labels.push(Cell.new(q["name"]))
        weight.push(Cell.new(q["weight"]))
      end
      sheet.columns.push(Col.new("", "Number"), Col.new("", "Percent"), Col.new("", "Percent"), Col.new("", "Percent"))
      labels.push(Cell.new("Total"), Cell.new("Computed%"), Cell.new("Curved"), Cell.new("OnServer%"))
      tot_weight = questions.map{|q| q["weight"]}.sum()
      weight.push(Cell.new(nil, Formula.new(tot_weight, "SUM",
                                            Range.new(col_name(weight.count - questions.count), true, 3, true,
                                                      col_name(weight.count - 1), true, 3, true))),
                  Cell.new(""), Cell.new(""), Cell.new(""))
      exam_cols.push [exam, weight.count - 2]

      users.each_with_index do |u, i|
        sub_id = subs_for_grading.find{|sfg| sfg.user_id == u.id}
        begin
          sub = grades.grades[:grades].find{|grade_row| grade_row[:sub].id == sub_id.submission_id} unless sub_id.nil?
        rescue Exception => e
          sub = nil
          Audit.log("Failed in query: #{grades.grades} and #{sub_id and sub_id.submission_id}\n#{e}\n")
        end
        if sub.nil?
          questions.each do |g| sheet.push_row(i, "") end
          sheet.push_row(i, [0, "No submission"])
          curved = Cell.new(nil,
                            CellRef.new(nil,
                                        col_name(weight.count - 3), true, i + sheet.header_rows.length + 2, false,
                                        nil))
          sheet.push_row(i, [curved, 0])
        else
          grade_comments = InlineComment.where(submission_id: sub_id.submission_id)
          q_grades = (0..questions.count-1).map{|i| grade_comments.find{|g| g.line == i}}.map{|c| (c && c["weight"])}
          q_grades.each do |g|
            sheet.push_row(i, g || "<none>")
          end
          sum_grade = Formula.new(nil, "SUM",
                                  Range.new(col_name(weight.count - questions.count - 4), true,
                                            i + sheet.header_rows.length + 2, false,
                                            col_name(weight.count - 5), true,
                                            i + sheet.header_rows.length + 2, false))
          sheet.push_row(i, Cell.new(nil, sum_grade))
          sum_grade = Formula.new(sub[:sub].score, "/",
                                  sum_grade, CellRef.new(nil, col_name(weight.count - 4), true, 3, true, tot_weight))

          sheet.push_row(i, Cell.new(nil, sum_grade))
          curve = grade_comments.find_by(line: questions.count)
          if curve
            curved = Cell.new(nil, Formula.new(sub[:sub].score, "/", curve["weight"], tot_weight))
          else
            curved = Cell.new(nil,
                              CellRef.new(nil,
                                          col_name(weight.count - 3), true, i + sheet.header_rows.length + 2, false,
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
      Col.new("LastName"), Col.new("FirstName"), Col.new("Instructor"),
      Col.new("NUID", "String"), Col.new("Email"),
      Col.new("Section", "Number"), Col.new("Withdrawn?", "DateTime"),
      Col.new(""), Col.new("")
    )
    labels = sheet.push_header_row(nil, ["", "", "", "", "", "", "", "", ""])
    weight = sheet.push_header_row(nil, ["", "", "", "", "", "", "", "", ""])

    users = course.students.order(:last_name, :first_name).to_a

    regs = course.registrations.includes(:section).to_a
    users.each do |u|
      reg = regs.find{|r| r.user_id == u.id}
      sheet.push_row(nil, [
                       u.last_name || u.name,
                       u.first_name || "",
                       reg.section.instructor.last_name,
                       u.nuid || "",
                       u.email,
                       reg.section.crn,
                       reg.dropped_date || "",
                       "", ""])
    end

    return labels, weight, users
  end
  
  def create_hws(course, sheet, exams)
    labels, weight, users = create_name_columns(course, sheet)

    hw_cols = []

    course.assignments.where.not(type: "exam").order(:due_date).each do |assn|
      used_subs = assn.all_used_subs.to_a
      grades = Gradesheet.new(assn, used_subs)
      subs_for_grading = UsedSub.where(assignment: assn).to_a
      
      sheet.columns.push(Col.new(assn.name, "Number"))
      grades.graders.each_with_index do |g, i|
        sheet.columns.push(Col.new("", "Number")) if i > 0
        labels.push(Cell.new(g.type))
        weight.push(Cell.new(g.avail_score))
      end
      sheet.columns.push(Col.new("", "Number"), Col.new("", "Percent"), Col.new("", "Percent"), Col.new("", "Percent"))
      labels.push(Cell.new("Total"), Cell.new("Lateness"), Cell.new("Computed%"), Cell.new("OnServer%"))
      tot_weight = grades.graders.map(&:avail_score).sum()
      weight.push(Cell.new(nil, Formula.new(tot_weight, "SUM",
                                            Range.new(col_name(weight.count - grades.graders.count), true, 3, true,
                                                      col_name(weight.count - 1), true, 3, true))),
                  Cell.new(""), Cell.new(""), Cell.new(""))
      hw_cols.push [assn, weight.count - 2]
      
      users.each_with_index do |u, i|
        sub_id = subs_for_grading.find{|sfg| sfg.user_id == u.id}
        sub = grades.grades[:grades].find{|grade_row| grade_row[:sub].id == sub_id.submission_id} unless sub_id.nil?
        if sub.nil?
          grades.graders.each do |g| sheet.push_row(i, "") end
          sheet.push_row(i, [0, "No submission", 0, 0])
        else
          sub[:staff_scores][:scores].each do |ss|
            sheet.push_row(i, ss[0] || "<none>")
          end
          sum_grade = Formula.new(sub[:sub].score, "SUM",
                                  Range.new(col_name(weight.count - grades.graders.count - 4), true,
                                            i + sheet.header_rows.length + 2, false,
                                            col_name(weight.count - 5), true,
                                            i + sheet.header_rows.length + 2, false))
          #sheet.push_row(i, sub[:staff_scores][:raw_score])
          sheet.push_row(i, Cell.new(nil, sum_grade))

          sum_grade = Formula.new(sub[:sub].score, "/",
                                  CellRef.new(nil,
                                              col_name(weight.count - 4), true,
                                              i + sheet.header_rows.length + 2, false,
                                              nil),
                                  CellRef.new(nil, col_name(weight.count - 4), true, 3, true, nil))

          if sub[:sub].ignore_late_penalty
            sheet.push_row(i, "<ignore>")
          else
            lc = assn.lateness_config
            sheet.push_row(i, lc.late_penalty(assn, sub[:sub]) / 100.0)
            sum_grade = Formula.new(nil, "MAX", 0,
                                    Formula.new(nil, "-", sum_grade,
                                                CellRef.new(nil,
                                                            col_name(weight.count - 3), true,
                                                            i + sheet.header_rows.length + 2, false,
                                                            lc.late_penalty(assn, sub[:sub]) / 100.0)
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
      sheet.columns.push(Col.new(assn.name, "Percent"))
      labels.push(Cell.new(assn.points_available / 100.0))
      weight.push(Cell.new(""))

      users.each_with_index do |u, i|
        sheet.push_row(i, Cell.new(nil, CellRef.new(hws.name, col_name(col), true, i + hw_headers, false,
                                                    "Please Recalculate")))
      end
    end

    exam_cols.each do |exam, col|
      sheet.columns.push(Col.new(exam.name, "Percent"))
      labels.push(Cell.new(exam.points_available / 100.0))
      weight.push(Cell.new(""))

      users.each_with_index do |u, i|
        sheet.push_row(i, Cell.new(nil, CellRef.new(exams.name, col_name(col), true, i + hw_headers, false,
                                                    "Please Recalculate")))
      end
    end

    end_col = sheet.columns.count - 1

    sheet.columns.push(Col.new("Total", "Percent"))
    labels.push(Cell.new(""))
    weight.push(Cell.new(""))

    users.each_with_index do |u, i|
      sheet.push_row(i, Cell.new(nil, Formula.new("Please recalculate", "SUMPRODUCT",
                                                  Range.new(col_name(start_col), true, 2, true,
                                                            col_name(end_col), true, 2, true),
                                                  Range.new(col_name(start_col), true, i + hw_headers, false,
                                                            col_name(end_col), true, i + hw_headers, false))))
    end

    sheet
  end

  def to_xlsx
    io = StringIO.new
    workbook = WriteXLSX.new(io)

    @twoPct = workbook.add_format
    @twoPct.set_num_format("0.00%")
    def render_row(ws, s, r, r_num, row_offset)
      r.each_with_index do |c, c_num|
        if s.columns[c_num].type == "Percent"
          format = @twoPct
        else
          format = nil
        end
        if c.formula
          ws.write_formula(r_num + row_offset, c_num, "=#{c.formula}", format)
        elsif c.value.is_a? Numeric
          ws.write_number(r_num + row_offset, c_num, c.value, format)
        elsif c.value.is_a? DateTime or c.value.is_a? Time
          ws.write_date_time(r_num + row_offset, c_num, c.value.to_formatted_s(:db), format)
        else
          ws.write(r_num + row_offset, c_num, c.value, format)
        end
      end
    end
    @sheets.each do |s|
      ws = workbook.add_worksheet(s.name)
      row_offset = 0
      s.columns.each_with_index do |c, c_num|
        ws.write(0 + row_offset, c_num, c.name)
      end
      row_offset += 1
      s.header_rows.each_with_index do |r, r_num|
        render_row(ws, s, r, r_num, row_offset)
      end
      row_offset += s.header_rows.count
      s.rows.each_with_index do |r, r_num|
        render_row(ws, s, r, r_num, row_offset)
      end
    end
    workbook.close
    io.string
  end
end
