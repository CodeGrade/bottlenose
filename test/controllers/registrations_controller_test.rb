require 'test_helper'

class RegistrationsControllerTest < ActionController::TestCase
  setup do
    make_standard_course
    @mike = create(:user)

    @cs301 = @cs101
  end

  test "should get index" do
    sign_in @fred
    get :index, params: {course_id: @cs301.id}
    assert_response :success
  end

  test "should not get index unless logged in" do
    get :index, params: {course_id: @cs301.id}
    assert_response :redirect
    assert_match "You need to log in first", flash[:alert]
  end

  test "non-teacher should not get index" do
    sign_in @john
    get :index, params: {course_id: @cs301.id}
    assert_response :redirect
    assert_match "Must be an admin, professor or assistant", flash[:alert]
  end

  test "should create registration" do
    sign_in @fred
    assert_difference('Registration.count') do
      post :create, params: {
             course_id: @cs101.id,
             new_sections: [@section.crn],
             registration: {
               username: @mike.username,
               course_id: @cs101.id,
               role: "student"
             }
           }
    end

    assert_response :redirect
  end

  test "should get new only when logged in as admin or staff" do
    get :new, params: {course_id: @cs301.id}
    assert_redirected_to new_user_session_path
    assert_match "You need to log in first", flash[:alert]

    sign_in @students[1]
    get :new, params: {course_id: @cs301.id}
    assert_redirected_to root_path
    assert_match "Must be an admin, professor or assistant", flash[:alert]
    sign_out @students[1]

    sign_in @fred
    get :new, params: {course_id: @cs301.id}
    assert_response :success
  end
  
  test "should highlight non-student registrations" do
    last_rr = nil
    Registration::roles.zip(@students) do |(roleName, role), student|
      rr = @cs301.reg_requests.new(user: student, role: role, "#{@section.type}_sections".to_sym => @section.crn.to_s)
      rr.save!
      @cs301.reg_requests << rr
      @cs301.save!
      res = rr.save
      puts "RESULT: #{res.to_s}"
      last_rr = rr
    end
    @cs301.save!
    puts "one: " + @cs301.reg_requests.count.to_s
    puts "two: " + last_rr.course.reg_requests.count.to_s
    @cs301.reload
    #assert_equal "expected", "result"
    assert_equal Registration::roles.count, @cs301.reg_requests.count
    sign_in @fred
    get :index, params: {course_id: @cs301.id}
    assert_response :success
    assert_select "table#reg-requests tr.sec_#{@section.crn}", Registration::roles.count
    assert_select "table#reg-requests tr.sec_#{@section.crn}.danger", 1
    assert_select "table#reg-requests tr.sec_#{@section.crn}.warning", 1
    assert_select "table#reg-requests tr.sec_#{@section.crn}.info", 1
  end
  
  test "should get bulk_edit" do
    sign_in @fred
    get :bulk_edit, params: {course_id: @cs301.id, role: "student"}
    assert_response :success
    sign_out @fred
  end

  test "should bulk_update registrations" do
    sign_in @fred
    post :bulk_update, params: {
          course_id: @cs301.id,
          id: @cs301.registrations.find_by(role: Registration::roles["student"]).id.to_s,
          orig_sections: [@section.crn.to_s],
          new_sections: [@section.crn.to_s],
          role: "student"
         },
         format: "json"
    assert_response :success
    assert JSON.parse(response.body)["no-change"]
    
    post :bulk_update, params: {
          course_id: @cs301.id,
          id: @cs301.registrations.find_by(role: Registration::roles["student"]).id.to_s,
          orig_sections: [@section.crn.to_s],
          new_sections: [@section.crn.to_s],
          role: "grader"
         },
         format: "json"
    assert_response :success
    assert_match "grader", JSON.parse(response.body)["reg"]["role"]
  end

  test "should destroy registration" do
    sign_in @fred
    assert_difference('Registration.count', -1) do
      delete :destroy, params: {course_id: @cs301.id, id: @john_reg.id}
    end

    assert_redirected_to course_registrations_path(@cs301)
  end

  def attempt_register_user(user, role, course=@cs101, section=@section)
    post :create, params: {
        course_id: course.id,
        registration: {
            username: user.username,
            role: role
        },
        new_sections: [section.crn.to_s]
    }
  end

  test "graders cannot create registrations" do
    mike_reg = Registration.create(course: @cs101,
                                   user: @mike,
                                   role: "grader",
                                   show_in_lists: false)
    mike_reg.save!
    sign_in @mike

    new_user = create(:user)
    attempt_register_user new_user, "grader"

    assert_redirected_to root_path
    assert_match "Must be an admin, professor or assistant.", flash[:alert]
    assert_empty @cs101.registrations.where(user: new_user)
  end

  test "professors can create registrations of any role" do
    sign_in @fred
    Registration.roles.each do |role, num|
      new_user = create(:user)
      attempt_register_user new_user, role
      assert_redirected_to course_registrations_path @cs101
      assert_nil flash[:alert]
      assert_equal role, @cs101.registrations.find_by(user: new_user).role
    end
  end

  test "TAs can only sign up students" do
    mike_reg = Registration.create(course: @cs101,
                                   user: @mike,
                                   role: "assistant",
                                   show_in_lists: false)
    mike_reg.save!
    sign_in @mike

    new_user = create(:user)

    attempt_register_user new_user, "student"
    assert_redirected_to course_registrations_path @cs101
    assert_nil flash[:alert]
    assert_equal "student", @cs101.registrations.find_by(user: new_user).role

    new_user = create(:user)

    %w(grader assistant professor).each do |role|
      attempt_register_user new_user, role
      assert_redirected_to course_registrations_path @cs101
      assert_match "You are not allowed to create #{role} registrations.", flash[:alert]
      assert_empty @cs101.registrations.where(user: new_user)
    end
  end

  test "self-registration disallowed" do
    mike_reg = Registration.create(course: @cs101,
                                   user: @mike,
                                   role: "assistant",
                                   show_in_lists: false)
    mike_reg.save!

    sign_in @mike
    attempt_register_user @mike, "professor"

    assert_redirected_to course_registrations_path @cs101
    assert_match "You are not allowed to create a registration for yourself.", flash[:alert]
    assert_equal "assistant", @cs101.registrations.find_by(user: @mike).role
  end

  test "registration downgrades disallowed" do
    sign_in @fred

    mike_reg = Registration.create(course: @cs101,
                                   user: @mike,
                                   role: "assistant",
                                   show_in_lists: false)
    mike_reg.save!

    # downgrade mike to a student
    attempt_register_user @mike, "student"

    assert_redirected_to course_registrations_path @cs101
    assert_match "You are not allowed to downgrade registrations.", flash[:alert]

    mike_reg.reload
    assert_equal "assistant", mike_reg.role
  end
end
