require 'test_helper'

class EditBulkAssignmentWeightTest < ActionDispatch::IntegrationTest
  include Devise::Test::IntegrationHelpers
  setup do
    Capybara.default_driver = :selenium_chrome_headless
    make_standard_course
    @pset = build(:assignment, course: @cs101, name: "Assignment 1", points_available: 25,teamset: @ts1)
    @pset.save!
    @pset2 = build(:assignment, course: @cs101, name: "Assignment 2", points_available: 25, teamset: @ts1)
    @pset2.save!
  end

  def create_exams
    @exam = Exam.new(course: @cs101, teamset: @ts1, due_date: Time.now, points_available: 25,
                    team_subs: false, lateness_config: @cs101.lateness_config, name: "Exam1",
                    available: Time.now - 1.days, blame: @fred)
    @exam.assignment_file = assign_upload_obj("Exam", "exam.yaml")
    g = ExamGrader.new(avail_score: 50, order: 1)
    @exam.graders << g
    @exam.save!
    @exam2 = Exam.new(course: @cs101, teamset: @ts1, due_date: Time.now, points_available: 25,
                    team_subs: false, lateness_config: @cs101.lateness_config, name: "Exam2",
                    available: Time.now - 1.days, blame: @fred)
    @exam2.assignment_file = assign_upload_obj("Exam", "exam.yaml")
    g = ExamGrader.new(avail_score: 50, order: 1)
    @exam2.graders << g
    @exam2.save!
  end  

  test "successfully updating weights of all assignments without any exams" do
    sign_in @fred
    visit weights_course_assignments_path(@cs101)
    fill_in(id: "weight_" + @pset.id.to_s , with: 50, fill_options: { clear: :backspace })
    fill_in(id: "weight_" + @pset2.id.to_s , with: 50, fill_options: { clear: :backspace })
    click_button('Update weights')
    visit weights_course_assignments_path(@cs101)
    assert_equal "50.0", find_field(id: "weight_" + @pset.id.to_s ).value
    assert_equal "50.0", find_field(id: "weight_" + @pset2.id.to_s ).value
  end

  test "successfully updating weights of all assignments" do
    create_exams
    sign_in @fred
    visit weights_course_assignments_path(@cs101)
    fill_in(id: "weight_" + @pset.id.to_s , with: 10, fill_options: { clear: :backspace })
    fill_in(id: "weight_" + @pset2.id.to_s , with: 30, fill_options: { clear: :backspace })
    click_button('Update weights')
    visit weights_course_assignments_path(@cs101)
    assert_equal "10.0", find_field(id: "weight_" + @pset.id.to_s ).value
    assert_equal "30.0", find_field(id: "weight_" + @pset2.id.to_s ).value
    assert_equal "25.0", find_field(id: "weight_" + @exam.id.to_s ).value
    assert_equal "25.0", find_field(id: "weight_" + @exam2.id.to_s ).value
  end

  test "successfully updating weights of all assignments and exams" do
    create_exams
    sign_in @fred
    visit weights_course_assignments_path(@cs101)
    fill_in(id: "weight_" + @pset.id.to_s , with: 20, fill_options: { clear: :backspace })
    fill_in(id: "weight_" + @pset2.id.to_s , with: 20, fill_options: { clear: :backspace })
    fill_in(id: "weight_" + @exam.id.to_s , with: 30, fill_options: { clear: :backspace })
    fill_in(id: "weight_" + @exam2.id.to_s , with: 30, fill_options: { clear: :backspace })
    click_button('Update weights')
    visit weights_course_assignments_path(@cs101)
    assert_equal "20.0", find_field(id: "weight_" + @pset.id.to_s ).value
    assert_equal "20.0", find_field(id: "weight_" + @pset2.id.to_s ).value
    assert_equal "30.0", find_field(id: "weight_" + @exam.id.to_s ).value
    assert_equal "30.0", find_field(id: "weight_" + @exam2.id.to_s ).value
  end
end
