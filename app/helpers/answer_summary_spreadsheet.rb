require 'gradesheet'
require 'audit'
require 'spreadsheet'

class AnswerSummarySpreadsheet < Spreadsheet
  def initialize(assn)
    @sheets = []
    @course = assn.course
    @assn = assn

    sheet = Sheet.new("Answers")
    labels, weight, @users = create_name_columns(@course, sheet)
    sheet.freeze_panes(sheet.header_rows.length + 1, 0)
    @sheets.push(sheet)

    repeats = assn.try(:review_count) || 1
    questions = @assn.flattened_questions
    (1..repeats).each do
      questions.each_with_index do |q, i|
        type = case q["type"]
               when "Numeric"
                 "Number"
               else
                 "String"
               end
        if q["name"]
          labels.push(Cell.new(q["name"]))
        else
          labels.push(Cell.new("Question #{i + 1}"))
        end
        weight.push(Cell.new(q["weight"]))
      end
    end

    subs_for_grading = UsedSub.where(assignment: assn).includes(submission: [:upload]).to_a
    subs_by_user = subs_for_grading.map{|sfg| [sfg.user_id, sfg.submission]}.to_h
    @users.each_with_index do |u, i|
      sub = subs_by_user[u.id]
      next if sub.nil?
      answers = YAML.load(File.open(sub.upload.submission_path))
      sub.answers.each do |_, answers|
        questions.zip(answers).each_with_index do |(q, a), qnum|
          response = case q["type"]
          when "Numeric"
            q["main"].to_f
          when "MultipleChoice"
            if (q["options"][a["main"].to_i] == "other")
              a["detail"]
            else
              q["options"][a["main"].to_i]
            end
          else
            a["main"]
          end
          sheet.push_row(i, Cell.new(response))
        end
      end
    end

    sheet.assign_coords
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
      row = [ sanitize(u.last_name || u.name || "<anonymous>"),
              sanitize(u.first_name || ""),
              sanitize(lecture&.dig(:section)&.instructor&.last_name || "<no instructor>"),
              u.nuid || "<###>",
              sanitize(u.username || "<no username>"),
              sanitize(u.email || "<no email>")]
      course_section_types.each do |type|
        section = reg[Section::types[type]]&.fetch(:section)
        row.push(section&.crn || "")
      end
      row.push(lecture&.dig(:dropped_date) || "", "", "")
      sheet.push_row(nil, row)
    end

    return labels, weight, users
  end
end
