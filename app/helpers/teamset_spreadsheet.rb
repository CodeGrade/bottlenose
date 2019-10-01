require 'gradesheet'
require 'audit'
require 'spreadsheet'

class TeamsetSpreadsheet < Spreadsheet
  def initialize(course)
    @sheets = []

    sheet = Sheet.new("Student information")
    labels, weight, users = create_name_columns(course, sheet)
    sheet.freeze_panes(sheet.header_rows.length + 1, 0)
    @sheets.push(sheet)

    @teams = course.teams.includes(:users).order(end_date: :desc, id: :asc).group_by(&:teamset_id)
    course.assignments_sorted.each do |a|
      next unless a.team_subs?
      sheet = Sheet.new("Teams for #{a.name}")
      sheet.columns.push(Col.new("ID", "Number"))
      sheet.columns.push(Col.new("Active"))
      sheet.columns.push(Col.new("Start date"))
      sheet.columns.push(Col.new("End date"))
      sheet.columns.push(Col.new("Users (name <email>)", "String"))
      sheet.freeze_panes(1, 0)
      (@teams[a.teamset_id] || []).each do |team|
        row = [team.id,
               (team.active? ? "Yes" : "No"),
               team.start_date&.iso8601,
               team.end_date&.iso8601 || "-----",
               *team.users.map do |u|
                 "#{u.display_name} <#{u.email}>"
               end]
        sheet.push_row(nil, row)
      end
      @sheets.push sheet
    end
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
