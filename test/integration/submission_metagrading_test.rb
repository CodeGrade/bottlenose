require 'test_helper'

class SubmissionMetagradingTest < ActionDispatch::IntegrationTest
  include Devise::Test::IntegrationHelpers

  setup do
    Capybara.default_driver = :selenium_chrome_headless
    make_standard_course
  end

  test "PDF metagrading score properly computed and loaded with comments on web page." do
    assignment = create(:assignment, name: "PDFs", course: @cs101, teamset: @ts1, team_subs: false,
      available: Date.current,
      due_date: Date.current + 1.day,
      points_available: 5);
    john_pdf_sub = make_submission(@john, assignment, "sample.pdf")

    # Only one grader/grade to compute with this submission.
    pdf_grader = assignment.graders.first
    john_pdf_grade = john_pdf_sub.grades.first
     
    # Creating a comment to ensure score is updated even with comments loaded.
    comment = InlineComment.new(user: @fred, submission: john_pdf_sub, grade: john_pdf_grade, 
      severity: InlineComment.severities[:warning], weight: 1.0, line: 1, title: "",
      comment: "You did not say \"Goodbye World!\" as well.", label: @fred.display_name, 
      filename: john_pdf_sub.file_path)
    comment.save!
  
    pdf_grader.grade(assignment, john_pdf_sub)
    john_pdf_grade.reload
    
    sign_in @kyle
    visit edit_course_assignment_submission_grade_path(@cs101, assignment, john_pdf_sub, john_pdf_grade)
    # save_and_open_page

    # Browser displays scores with two trailing zeroes (after decimal), 
    # as well as two spaces b/n the slash and the "out_of" value.
    score_text = find("span#score").text
    assert_equal sprintf("%.2f", john_pdf_grade.score), score_text
  end
end