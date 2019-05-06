require 'test_helper'

class ShowHideUsernameTest < ActionDispatch::IntegrationTest
  include Devise::Test::IntegrationHelpers
  setup do
    Capybara.default_driver = :selenium_chrome_headless
    make_standard_course
    @pset = build(:assignment, course: @cs101, name: "Assignment 1", teamset: @ts1, blame: @fred)
    @pset.save!
    @sub = make_submission(@john, @pset, "sample.rkt")
    @sub.set_used_sub!
  end

  test "professor checking if show and hide of username works on assignments page" do
    sign_in @fred
    visit course_assignment_path(@cs101, @pset)
    assert page.has_selector?(:css, '#show_username') 
    assert_equal 0, find_all(:css, '.username', visible: true).count()
    click_button('show_username')
    assert_equal 0, find_all(:css, '.username', visible: :hidden).count()
    click_button('show_username')
    assert_equal 1, find_all(:css, '.username', visible: :hidden).count()
  end
end
