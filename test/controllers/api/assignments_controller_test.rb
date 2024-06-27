require 'test_helper'

module Api
  class AssignmentsControllerTest < ActionController::TestCase
    setup do
      make_standard_course
      @application = Doorkeeper::Application.create!(name: 'test app', redirect_uri: 'https://localhost', owner: User.first)

      @assn_no_teams_1 = create(:assignment, course: @cs101, teamset: @ts1, name: "HelloWorld")
      @assn_shared_teams_1 = create(:assignment, course: @cs101, teamset: @ts2, team_subs: true, name: "TestScript")
      @assn_shared_teams_2 = create(:assignment, course: @cs101, teamset: @ts2, team_subs: true)
      @assn_teams_1 = create(:assignment, course: @cs101, teamset: @ts3, team_subs: true, name: "HelloSingle")
      
      # Create some non-active teams, and some active ones
      @ts2.randomize(2, "course", Date.current - 1.week, Date.current - 1.day)
      @ts2.randomize(2, "course", Date.current)

      @ts3.randomize(1, "course", Date.current)
    end
    teardown do
      Upload.cleanup_test_uploads!
    end

    def sign_in(user)
      super(user)
      token = Doorkeeper::AccessToken.create!(application_id: @application.id, resource_owner_id: user.id, scopes: 'all')
      @controller.instance_variable_set('@doorkeeper_token', token)
    end
    
    def create_exam_for(course, name, existing_id = nil)
      summary = make_random_summary
      {
        course_id: course.id,
        name: name,
        exam_id: existing_id,
        finish_time: DateTime.current,
        exam_summary: summary,
        exam_grades: make_random_grades(course, summary)
      }.compact
    end

    def make_random_summary
      num_qs = 1 + rand(5)
      (1..num_qs).map do |qnum|
        num_ps = 1 + rand(3)
        if num_ps == 1
          {
            name: "Q#{qnum}",
            weight: rand(10),
            extra: rand(10) == 0
          }
        else
          {
            name: "Q#{qnum}",
            parts: (1..num_ps).map do |pnum|
              {
                name: "Q#{qnum}-P#{pnum}",
                weight: rand(10),
                extra: rand(10) == 0
              }
            end
          }
        end
      end
    end

    def make_random_grades(course, summary)
      course.students.to_h do |s|
        [
          s.username,
          summary.map do |q|
            if q[:parts].nil?
              q[:weight] == 0 ? 0 : rand(q[:weight])
            else
              q[:parts].map { |p| p[:weight] == 0 ? 0 : rand(p[:weight]) }
            end
          end
        ]
      end
    end

    test "should create exam" do
      exam_data = create_exam_for(@cs101, "New exam")
      sign_in @fred

      assert_difference('Assignment.count') do
        post :create_or_update, params: exam_data, as: :json
        assert_response 200
      end
      exam_records = collect_exam_records(Exam.last)

      assert_equal_exam exam_records, exam_data[:exam_grades]
    end

    test "should update exam" do
      exam_data = create_exam_for(@cs101, "New exam")
      sign_in @fred

      assert_difference('Assignment.count') do
        post :create_or_update, params: exam_data, as: :json
        assert_response 200
      end

      old_exam_records = collect_exam_records(Exam.last)
      assert_equal_exam old_exam_records, exam_data[:exam_grades]
      
      exam_data[:exam_id] = old_exam_records[:exam].id
      # Repost the exact same exam ==> should destroy and recreate all records
      assert_no_difference ['InlineComment.count', 'Grade.count', 'Submission.count'] do
        post :create_or_update, params: exam_data, as: :json
        assert_response 200
      end
      assert Submission.where(id: old_exam_records[:subs].values.flatten.map(&:id)).blank?
      assert UsedSub.where(id: old_exam_records[:used_subs].values.flatten.map(&:id)).blank?
      assert UserSubmission.where(id: old_exam_records[:user_submissions].values.flatten.map(&:id)).blank?
      assert Grade.where(id: old_exam_records[:grades].values.flatten.map(&:id)).blank?
      assert InlineComment.where(id: old_exam_records[:comments].values.flatten.map(&:id)).blank?
      mid_exam_records = collect_exam_records(Exam.last)
      assert_equal_exam mid_exam_records, exam_data[:exam_grades]

      # Create new data
      exam_data[:exam_grades] = make_random_grades(@cs101, exam_data[:exam_summary])
      assert_no_difference ['InlineComment.count', 'Grade.count', 'Submission.count'] do
        post :create_or_update, params: exam_data, as: :json
        assert_response 200
      end
      new_exam_records = collect_exam_records(Exam.last)
      assert_equal_exam new_exam_records, exam_data[:exam_grades]      
    end

    def collect_exam_records(exam)
      subs = Submission.where(assignment: exam).includes(:user)
      used_subs = UsedSub.where(submission: subs).group_by(&:submission_id)
      user_submissions = UserSubmission.where(submission: subs).group_by(&:submission_id)
      comments = InlineComment.where(submission: subs).order(line: :asc).group_by(&:submission_id)
      grades = Grade.where(submission: subs).group_by(&:submission_id)
      {
        exam: exam,
        subs: subs.group_by(&:user_id),
        used_subs: used_subs,
        user_submissions: user_submissions,
        comments: comments,
        grades: grades
      }
    end

    def assert_equal_exam(records, grades)
      assert_equal records[:subs].count, grades.count
      assert_equal records[:used_subs].count, grades.count
      assert_equal records[:user_submissions].count, grades.count
      assert_equal records[:grades].count, grades.count
      records[:subs].each do |uid, subs|
        assert_equal 1, subs.count
        sub = subs.first
        assert_equal 1, records[:used_subs][sub.id].count
        assert_equal 1, records[:user_submissions][sub.id].count
        assert_equal 1, records[:grades][sub.id].count
        grades_for = grades[sub.user.username].flatten
        assert_equal grades_for.count, records[:comments][sub.id].count
        grades_for.zip(records[:comments][sub.id]).each do |g, c|
          assert_equal g, c.weight
        end
      end
    end
  end
end
