class SchematizeTerms < ActiveRecord::Migration[5.2]
  def up
    add_column :terms, :semester, :integer, default: 0
    add_column :terms, :year, :integer, default: 0
    Term.all.each do |t|
      ym = /(.*) (\d\d\d\d)/.match(t.read_attribute(:name))
      t.year = ym[2]
      case ym[1].downcase
      when "fall"
        t.semester = Term.semesters[:fall]
      when "winter"
        t.semester = Term.semesters[:winter]
      when "spring"
        t.semester = Term.semesters[:spring]
      when "summer 1", "summer1", "summer i"
        t.semester = Term.semesters[:summer_1]
      when "summer 2", "summer2", "summer ii"
        t.semester = Term.semesters[:summer_2]
      when "summer"
        t.semester = Term.semesters[:summer]
      end
      t.save
    end
    change_column_null :terms, :semester, false
    change_column_null :terms, :year, false
    remove_column :terms, :name
  end

  def down
    add_column :terms, :name, :string
    Term.all.each do |t|
      t.name = "#{t.semester.humanize} #{t.year}"
      t.save
    end
    remove_column :terms, :semester
    remove_column :terms, :year
  end
end
