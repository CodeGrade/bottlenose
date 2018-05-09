class Term < ApplicationRecord
  has_many :courses, :dependent => :restrict_with_error

  validates :name, :presence => true, :format => { :with => /\d\d/ }

  def self.all_sorted
    terms = Term.all
    terms.sort_by {|tt| tt.canonical_name }.reverse
  end

  def canonical_name
    ym   = /\d\d\d\d/.match(name)
    year = ym.nil? ? nil : ym[0]

    if year.nil? && (ym = /\d\d/.match(name))
      year = "20#{ym[0]}"
    end

    if year.nil?
      raise Exception.new("Cannot canonicalize name")
    end

    season = "0_Top"
    season = "1_Winter" if name =~ /winter/i
    season = "2_Spring" if name =~ /spring/i
    season = "3_Summer" if name =~ /summer/i
    season = "4_Fall"   if name =~ /fall/i

    arch = archived? ? "a" : "z"

    "#{arch} #{year} #{season} #{name}"
  end
end
