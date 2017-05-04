require 'test_helper'

class TermTest < ActiveSupport::TestCase
  setup do
    create(:term, name: "14 Summer")
    create(:term, name: "12 FALL")
    create(:term, name: "Spring 2014")
    create(:term, name: "14 Fall")
  end


  test "term names are canonicalized as expected" do
    tt = Term.new(name: "Fall 2012")
    assert_equal "z 2012 4_Fall Fall 2012", tt.canonical_name

    tt = Term.new(name: "2012 Spring")
    assert_equal "z 2012 2_Spring 2012 Spring", tt.canonical_name

    tt = Term.new(name: "summer 12")
    assert_equal "z 2012 3_Summer summer 12", tt.canonical_name

    tt = Term.new(name: "44 WINTER")
    assert_equal "z 2044 1_Winter 44 WINTER", tt.canonical_name

    tt = Term.new(name: "99 Bacon", archived: true)
    assert_equal "a 2099 0_Top 99 Bacon", tt.canonical_name
  end

  test "terms are sorted as expected" do
    assert_equal ["14 Fall", "14 Summer", "Spring 2014", "12 FALL"],
      Term.all_sorted.map {|tt| tt.name}
  end
end
