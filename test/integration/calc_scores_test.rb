require 'test_helper'
require 'pry'

# This is a long form sanity test for score calculation.
# Specifically, I want to make sure that BestSubs go to
# the correct assignments, and buckets add up correctly.

class CalcScoresTest < ActionDispatch::IntegrationTest
  setup do
    DatabaseCleaner.clean
    Capybara.current_driver = :webkit
  end

  def test_create_teams_submit_and_grade_many
    skip
    DatabaseCleaner.clean_with :truncation

    # Staff
    @ken     = create(:admin_user)
    @fred    = create(:user, name: "Fred McTeacher")

    # Students
    @jane    = create(:user, name: "Jane McStudent")
    @mary    = create(:user, name: "Mary McStudent")
    @john    = create(:user, name: "John McStudent")
    @mark    = create(:user, name: "Mark McStudent")

    # Course 1
    @cs101   = create(:course, name: "CS 101", public: true)
    @hw101   = create(:bucket, name: "Homework", course: @cs101, weight: 0.5)
    @qz101   = create(:bucket, name: "Quizzes", course: @cs101, weight: 0.5)
    @fred101 = create(:registration, course: @cs101, user: @fred, teacher: true)
    @jane101 = create(:registration, course: @cs101, user: @jane)
    @mary101 = create(:registration, course: @cs101, user: @mary)
    @john101 = create(:registration, course: @cs101, user: @john)
    @mark101 = create(:registration, course: @cs101, user: @mark)

    @cs101hw1 = create(:assignment, name: "cs1hw1", course: @cs101, bucket: @hw101, teamset: @ts1)
    @cs101hw2 = create(:assignment, name: "cs1hw2", course: @cs101, bucket: @hw101, teamset: @ts1, team_subs: true)
    @cs101qz1 = create(:assignment, name: "cs1qz1", course: @cs101, bucket: @qz101, teamset: @ts1)
    @cs101qz2 = create(:assignment, name: "cs1qz2", course: @cs101, bucket: @qz101, teamset: @ts1)

    # Course 2
    @cs102   = create(:course, name: "CS 102", public: false)
    @hw102   = create(:bucket, name: "Homework", course: @cs102, weight: 0.5)
    @qz102   = create(:bucket, name: "Quizzes", course: @cs102, weight: 0.5)
    @fred102 = create(:registration, course: @cs102, user: @fred, teacher: true)
    @jane102 = create(:registration, course: @cs102, user: @jane)
    @mary102 = create(:registration, course: @cs102, user: @mary)
    @john102 = create(:registration, course: @cs102, user: @john)
    @mark102 = create(:registration, course: @cs102, user: @mark)

    @cs102hw1 = create(:assignment, name: "cs2hw1", course: @cs102, bucket: @hw102, teamset: @ts2)
    @cs102hw2 = create(:assignment, name: "cs2hw2", course: @cs102, bucket: @hw102, teamset: @ts2, team_subs: true)
    @cs102qz1 = create(:assignment, name: "cs2qz1", course: @cs102, bucket: @qz102, teamset: @ts2)
    @cs102qz2 = create(:assignment, name: "cs2qz2", course: @cs102, bucket: @qz102, teamset: @ts2)


    @tars_dir = Rails.root.join('test', 'fixtures', 'files')
    @upload_file = @tars_dir.join('HelloSingle', 'hello.c')

    # Begin Entering Data
    enter_grade(@cs102qz1, @jane, 45)
    enter_grade(@cs102qz1, @mary, 61)
    enter_grade(@cs102qz1, @john, 82)
    enter_grade(@cs102qz1, @mark, 67)

    create_team(@cs102, [@jane, @mary])
    create_team(@cs102, [@mark, @john])
    create_team(@cs101, [@john, @mary])
    create_team(@cs101, [@mark, @jane])

    submit_work(@cs102hw1, @mark, @upload_file)
    submit_work(@cs102hw1, @jane, @upload_file)
    submit_work(@cs102hw1, @mary, @upload_file)
    submit_work(@cs102hw1, @john, @upload_file)

    submit_work(@cs101hw2, @mary, @upload_file)
    submit_work(@cs101hw2, @mark, @upload_file)

    submit_work(@cs102hw2, @mary, @upload_file)
    submit_work(@cs102hw2, @mark, @upload_file)
    submit_work(@cs102hw2, @jane, @upload_file)
    submit_work(@cs102hw2, @john, @upload_file)

    submit_work(@cs101hw1, @mark, @upload_file)
    submit_work(@cs101hw1, @jane, @upload_file)
    submit_work(@cs101hw1, @mary, @upload_file)
    submit_work(@cs101hw1, @john, @upload_file)

    [@jane, @mary, @john, @mark].each do |user|
      enter_grade(@cs102qz2, user, 100)
    end

    [@jane, @mary, @john, @mark].each do |user|
      enter_grade(@cs101qz1, user, 100)
    end

    [@jane, @mary, @john, @mark].each do |user|
      enter_grade(@cs101qz2, user, 50)
    end

    [@jane, @mary, @john, @mark].each do |user|
      enter_grade(@cs101hw1, user, 100)
    end

    [@jane, @mary, @john, @mark].each do |user|
      enter_grade(@cs102hw1, user, 100)
    end

    [@mary, @mark].each do |user|
      enter_grade(@cs101hw2, user, 100)
    end

    enter_grade(@cs102hw2, @mary, 75)
    enter_grade(@cs102hw2, @mark, 25)

    expected_grades = [
      [@cs101, @jane, 87.5],
      [@cs101, @mary, 87.5],
      [@cs101, @john, 87.5],
      [@cs101, @mark, 87.5],
      [@cs102, @jane, 80],
      [@cs102, @mary, 84],
      [@cs102, @john, 76.75],
      [@cs102, @mark, 73],
    ]

    expected_grades.each do |row|
      cc, uu, gg = row
      check_grade_teacher(cc, uu, gg)
      check_grade_student(cc, uu, gg)
    end
  end

  private

  def check_grade_student(course, user, grade_expected)
    login_as(user)
    click_link course.name
    click_link "Status Report"

    grade_shown = find("#total-score").text
    diff = (grade_shown.to_i - grade_expected).abs
    assert diff < 2, "Grade mismatch, student check for #{user.name}, diff = #{diff}"
  end

  def check_grade_teacher(course, user, grade_expected)
    login_as(@fred)
    click_link course.name

    row = find("#students-summary td", text: user.name).find(:xpath, "..")
    grade_shown = row.find(".score-total").text

    diff = (grade_shown.to_i - grade_expected).abs
    assert diff < 2, "Grade mismatch, teacher check for #{user.name}, diff = #{diff}"
  end

  def submit_work(assign, user, file)
    course = assign.course

    login_as(user)

    click_link course.name
    click_link assign.name
    if has_content? "New Submission"
      click_link "New Submission"
    else
      click_link "New Team Submission"
    end

    fill_in 'Student notes', with: 'This is a submission'
    attach_file 'Upload', file
    click_button 'Create Submission'
  end

  def create_team(course, users)
    login_as(@fred)

    click_link 'Your Courses'
    click_link course.name

    first(:link, 'Teams').click
    click_link('New Team')

    users.each do |uu|
      find('tr', text: uu.name).find('.add-user-btn').click
    end

    click_button("Create Team")

    assert has_content?("Team was successfully created.")
    users.each do |uu|
      assert has_content?(uu.name)
    end
  end

  def enter_grade(assign, user, grade)
    course = assign.course
    reg = user.registration_for(course) or raise Exception.new("User: #{user.name}, Course: #{course.name}")
    old_score = reg.total_score

    login_as(@fred)

    click_link 'Your Courses'
    click_link course.name
    click_link assign.name

    row = find("td", text: user.name).find(:xpath, "..")
    row.fill_in("submission[teacher_score]", with: grade)
    row.click_button row.find('input[type=submit]').value
    assert find("#ajax-status").has_content?("ajax-status: done")

    assert find("td", text: user.name).find(:xpath, "..").has_content?("#{grade}")
    assert reg.total_score > old_score, "Didn't increase grade for #{assign.name}, #{user.name}"
  end
end
