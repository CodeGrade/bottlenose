require 'test_helper'

class TermTest < ActiveSupport::TestCase
  setup do
    Term.semesters.each do |semester, sem_val|
      [2011, 2012, 2015].each do |year|
        create(:term, archived: true, year: year, semester: sem_val)
      end
      [2013, 2014].each do |year|
        create(:term, year: year, semester: sem_val)
      end
    end
  end


  test "term names are canonicalized as expected" do
    tt = Term.new(year: 3013, semester: Term.semesters[:fall])
    assert_equal "z 3014 10_fall Fall 3013", tt.canonical_name # Note sorting year and actual year differ

    tt = Term.new(year: 3013, semester: Term.semesters[:spring])
    assert_equal "z 3013 30_spring Spring 3013", tt.canonical_name

    tt = Term.new(year: 3013, semester: Term.semesters[:summer])
    assert_equal "z 3013 50_summer Summer 3013", tt.canonical_name

    tt = Term.new(year: 3044, semester: Term.semesters[:summer_2])
    assert_equal "z 3044 60_summer_2 Summer 2 3044", tt.canonical_name
  end

  test "terms are sorted as expected" do
    assert_equal ["Fall 2014", "Summer 2 2014", "Summer 2014", "Summer 1 2014", "Spring 2014",
                  "Fall 2013", "Summer 2 2013", "Summer 2013", "Summer 1 2013", "Spring 2013",
                  # These semesters are archived, so they sort "after" the active semesters,
                  # even though 2015 > 2014
                  "Fall 2015", "Summer 2 2015", "Summer 2015", "Summer 1 2015", "Spring 2015",
                  "Fall 2012", "Summer 2 2012", "Summer 2012", "Summer 1 2012", "Spring 2012",
                  "Fall 2011", "Summer 2 2011", "Summer 2011", "Summer 1 2011", "Spring 2011"],
      Term.all_sorted.map(&:name)
  end

  test "terms have query codes as expected" do
    query_codes = Term.all_sorted.map(&:query_code)
    sorted_codes = query_codes.sort_by do |a, b|
      ai = a.to_i
      bi = b.to_i
      ai -= 100 if (ai % 100) < 30 # Fall gets bumped by a year
      bi -= 100 if (bi % 100) < 30 # so deducting 100 == decrementing the year
      ai <=> bi
    end
    assert_equal query_codes, sorted_codes
  end
end
