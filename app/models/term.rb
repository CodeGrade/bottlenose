class Term < ApplicationRecord
  # For use with querying university rosters: numbers are set by university,
  # and there are a few other semester types, but we never use them...
  enum semester: { fall: 10, spring: 30, summer_1: 40, summer: 50, summer_2: 60 }
  has_many :courses, :dependent => :restrict_with_error

  validates :semester, inclusion: {in: Term.semesters.keys},
            uniqueness: {
              scope: :year,
              message: ->(object, data) do
                "Terms must be unique, but the semester/year pair <code>#{object.name}</code> already exists"
              end}
  
  def self.all_sorted
    terms = Term.all
    terms.sort_by {|tt| tt.canonical_name }.reverse
  end

  def name
    "#{semester.humanize} #{year}"
  end

  def canonical_name
    season = "#{Term.semesters[semester]}_#{semester}"

    arch = archived? ? "a" : "z"

    "#{arch} #{effective_year} #{season} #{name}"
  end

  def query_code
    # For use with querying university rosters
    "#{effective_year}#{Term.semesters[semester]}"
  end

  private
  def effective_year
    # Fall is part of numerically-next *academic* year
    year + ((Term.semesters[semester] < Term.semesters[:spring]) ? 1 : 0)
  end
end
