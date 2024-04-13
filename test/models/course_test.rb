require 'test_helper'
include ActionDispatch::TestProcess

class CourseTest < ActiveSupport::TestCase
  setup do
    @fred = create(:user, name: "Fred McTeacher", first_name: "Fred", last_name: "McTeacher", nickname: "Fred")
    @term = Term.create(semester: Term.semesters[:fall], year: Date.current.year, archived: false)
    @late_per_day = LatePerDayConfig.create(days_per_assignment: 1, percent_off: 50,
                                            frequency: 1, max_penalty: 100)
    @course = Course.new(name: "Computing 101", term: @term, lateness_config: @late_per_day)
    @sections = (1..3).map do |_|
      build(:section, course: @course, instructor: @fred)
    end
    @course.sections = @sections
    @course.save
    @fred_reg = Registration.create(course: @course, user: @fred, role: Registration::roles[:professor],
                                    show_in_lists: false, new_sections: @sections)
    @fred_reg.save_sections

    @students = []
    (1..60).each do |_|
      student = create(:user)
      reg = Registration.create(course: @course, user: student,
                                role: Registration::roles[:student], show_in_lists: true,
                                new_sections: [@sections.sample])
      reg.save_sections
      @students << student
    end

    
    @assignments = []
    # ASSIGNMENT 1
    ts1 = Teamset.create(course: @course, name: "Teamset 1")
    assn = Files.create(name: "Assignment 1", blame: @fred, teamset: ts1, lateness_config: @late_per_day,
                        course: @course, available: Time.current - 15.days, due_date: Time.current - 10.days,
                        points_available: 2.5)
    assn.graders << RacketStyleGrader.new(assignment: assn, params: "80", avail_score: 30, order: 1)
    assn.graders << ManualGrader.new(assignment: assn, avail_score: 50, order: 2)
    assn.save!
    @assignments << assn


    # ASSIGNMENT 2
    ts2 = Teamset.create(course: @course, name: "Teamset 2")
    ts2.randomize(2, "course", Time.current - 10.days)
    assn = Files.create(name: "Assignment 2", blame: @fred, teamset: ts2, lateness_config: @late_per_day,
                        course: @course, available: Time.current - 10.days, due_date: Time.current - 5.days,
                        points_available: 2.5, team_subs: true)
    u = build(:upload, user: @fred, assignment: assn)
    u.upload_data = FakeUpload.new(Rails.root.join("test", "fixtures", "files", "fundies-config.json").to_s)
    assn.graders << JavaStyleGrader.new(assignment: assn, avail_score: 30, order: 1, upload: u)
    u = build(:upload, user: @fred, assignment: assn)
    u.upload_data = FakeUpload.new(Rails.root.join("test", "fixtures", "files", "Assignment 2", "checker-tests.java").to_s)
    g = CheckerGrader.new(assignment: assn, upload: u, avail_score: 50, order: 2)
    g.test_class = "ExamplesMobilesReference"
    g.errors_to_show = 3
    g.test_timeout = 10
    assn.graders << g
    assn.graders << ManualGrader.new(assignment: assn, avail_score: 50, order: 3)
    assn.save!
    @assignments << assn


  
    # ASSIGNMENT 3
    ts3 = Teamset.create(course: @course, name: "Teamset 3")
    ts3.randomize(3, "course", Time.current - 10.days)
    assn = Files.create(name: "Assignment 3", blame: @fred, teamset: ts3, lateness_config: @late_per_day,
                        course: @course, available: Time.current - 5.days, due_date: Time.current - 1.days,
                        points_available: 2.5, team_subs: true)
    u = build(:upload, user: @fred, assignment: assn)
    u.upload_data = FakeUpload.new(Rails.root.join("test", "fixtures", "files", "fundies-config.json").to_s)
    assn.graders << JavaStyleGrader.new(assignment: assn, avail_score: 30, order: 1, upload: u)
    assn.graders << ManualGrader.new(assignment: assn, avail_score: 50, order: 3)
    assn.save!
    @assignments << assn  
  end


  def create_submission(course, students, assn, file, count = 1)
    students.to_a.sample(count).map do |user|
      sub = FilesSub.create(assignment: assn, user: user,
                            team: user.active_team_for(course, assn), created_at: assn.available + 1.5.days)
      sub.upload_file = fixture_file_upload("#{assn.name}/#{file}", "application/octet-stream")
      sub.save_upload
      sub.save!
      sub.set_used_everyone!
      sub
    end
  end

  def grade_sub(sub, graders, complete)
    graders.each do |g| g.ensure_grade_exists_for! sub end
    sub.grades.each do |g|
      g.score = Random.rand(g.out_of)
      g.save
    end
    sub.compute_grade! if complete
  end

  test "missing grades should be pending" do
    create_submission(@course, @students, @assignments[2], "Mobiles.java", 15)
    graders = @assignments[2].graders.to_a
    @assignments[2].submissions.each do |s|
      grade_sub(s, graders[0...-1], false)
    end
    # Not all grades created, so these submissions should show up in missing
    assert_equal @assignments[2].used_submissions.map(&:id).to_set, @course.missing_grading.map(&:id).to_set
    # And pending should be empty
    assert_equal [].to_set, (@course.pending_grading[@assignments[2].id]&.to_set || [].to_set)

    @assignments[2].submissions.each do |s|
      graders[-1].ensure_grade_exists_for! s
    end
    # Nothing's missing any more
    assert_equal [].to_set, @course.missing_grading.map(&:id).to_set
    # so everything should be pending
    assert_equal @assignments[2].used_submissions.map(&:id).to_set, @course.pending_grading[@assignments[2].id].keys.to_set
    # but nothing should be published
    assert_equal [].to_set, (@course.unpublished_grades[@assignments[2].id]&.to_set || [].to_set)
  end

  test "unpublished subs should not be missing grades" do
    create_submission(@course, @students, @assignments[2], "Mobiles.java", 15)
    graders = @assignments[2].graders.to_a
    @assignments[2].submissions.each do |s| grade_sub(s, graders, true) end

    @course.reload

    assert_equal [], @course.missing_grading.to_a
    assert_equal [], @course.pending_grading.to_a
    assert_equal [].to_h, @course.abnormal_subs
    assert_equal @assignments[2].used_submissions.map(&:id).to_set, @course.unpublished_grades[@assignments[2].id].map(&:id).to_set
  end

  test "disolving a group should make homework abnormal" do
    create_submission(@course, @course.students, @assignments[1], "Mobiles.java", 15)
    @assignments[1].submissions.each do |s| grade_sub(s, @assignments[1].graders.to_a, true) end
    team = @assignments[1].used_submissions[0].team
    team.dissolve(@assignments[1].due_date - 1.day)

    assert_equal 1, @course.abnormal_subs.count
  end

end
