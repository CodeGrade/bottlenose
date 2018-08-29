require 'test_helper'

class AssignmentsControllerTest < ActionController::TestCase
  setup do
    make_standard_course

    @assn_no_teams_1 = create(:assignment, course: @cs101, teamset: @ts1, name: "HelloWorld")
    @assn_shared_teams_1 = create(:assignment, course: @cs101, teamset: @ts2, team_subs: true, name: "TestScript")
    @assn_shared_teams_2 = create(:assignment, course: @cs101, teamset: @ts2, team_subs: true)
    @assn_teams_1 = create(:assignment, course: @cs101, teamset: @ts3, team_subs: true, name: "HelloSingle")
    
    # Create some non-active teams, and some active ones
    @ts2.randomize(2, "course", Date.today - 1.week, Date.today - 1.day)
    @ts2.randomize(2, "course", Date.today)

    @ts3.randomize(1, "course", Date.today)
  end

  test "should get new" do
    sign_in @fred
    get :new, params: {course_id: @cs101.id}
    assert_response :success
  end

  test "should create non-team assignment" do
    sign_in @fred
    teamset_count = Teamset.count
    assert_difference('Assignment.count') do
      post :create, params: {
             course_id: @cs101.id,
             assignment: {
               teamset_plan: "none",
               assignment: "Dance a jig.",
               points_available: 100,
               name: "Useful Work",
               due_date: '2019-05-22',
               available: '2011-05-22',
               type: "Files",
               graders_attributes: {
                 "1477181088065"=> {
                   "type"=>"ManualGrader",
                   "avail_score"=>"50",
                   "order"=>"1",
                   "_destroy"=>"false",
                 }
               },
               lateness_config_attributes: {
                 "type"=>"LatePerHourConfig",
                 "frequency"=>"1",
                 "percent_off"=>"25",
                 "max_penalty"=>"100",
                 "days_per_assignment"=>"22"
               },
             }
           }
    end

    @assn = assigns(:assignment)
    assert_redirected_to [@cs101, @assn.becomes(Assignment)]
    assert_equal teamset_count + 1, Teamset.count
    assert_equal 0, @assn.teamset.teams.count
  end

  test "should create assignment with no preset teams" do
    sign_in @fred
    teamset_count = Teamset.count
    assert_difference('Assignment.count') do
      post :create, params: {
             course_id: @cs101.id,
             assignment: {
               teamset_plan: "none",
               assignment: "Dance a jig.",
               points_available: 100,
               name: "Useful Work",
               due_date: '2019-05-22',
               available: '2011-05-22',
               type: "Files",
               graders_attributes: {
                 "1477181088065"=> {
                   "type"=>"ManualGrader",
                   "avail_score"=>"50",
                   "order"=>"1"
                 }
               },
               lateness_config_attributes: {
                 "type"=>"LatePerHourConfig",
                 "frequency"=>"1",
                 "percent_off"=>"25",
                 "max_penalty"=>"100",
                 "days_per_assignment"=>"22"
               },
             }
           }
    end

    @assn = assigns(:assignment)
    assert_redirected_to [@cs101, @assn.becomes(Assignment)]
    assert_equal teamset_count + 1, Teamset.count
    assert_equal 0, @assn.teamset.teams.count
  end

  test "should create assignment using existing teamset" do
    sign_in @fred
    teamset_count = Teamset.count
    assert_difference('Assignment.count') do
      post :create, params: {
             course_id: @cs101.id,
             assignment: {
               teamset_plan: "use",
               teamset_source_use: @ts2.id,
               assignment: "Sharing is caring",
               points_available: 100,
               name: "Using existing teamset",
               due_date: "2017-07-04",
               available: "2017-06-20",
               type: "Files",
               graders_attributes: {
                 "1477181088065"=> {
                   "type"=>"ManualGrader",
                   "avail_score"=>"50",
                   "order"=>"1"
                 }
               },
               lateness_config_attributes: {
                 "type"=>"LatePerHourConfig",
                 "frequency"=>"1",
                 "percent_off"=>"25",
                 "max_penalty"=>"100",
                 "days_per_assignment"=>"22"
               },
             }
           }
    end
               
    @asn = assigns(:assignment)
    assert_redirected_to [@cs101, @asn.becomes(Assignment)]

    assert_equal @ts2.id, @asn.teamset_id
    assert_equal teamset_count, Teamset.count
  end
  
  test "should create assignment copying existing teamset" do
    sign_in @fred
    teamset_count = Teamset.count
    assert_difference('Assignment.count') do
      post :create, params: {
             course_id: @cs101.id,
             assignment: {
               teamset_plan: "copy",
               teamset_source_copy: @ts2.id,
               assignment: "Copying with attribution is great",
               points_available: 100,
               name: "Copy existing teamset",
               due_date: "2017-07-04",
               available: "2017-06-20",
               type: "Files",
               graders_attributes: {
                 "1477181088065"=> {
                   "type"=>"ManualGrader",
                   "avail_score"=>"50",
                   "order"=>"1"
                 }
               },
               lateness_config_attributes: {
                 "type"=>"LatePerHourConfig",
                 "frequency"=>"1",
                 "percent_off"=>"25",
                 "max_penalty"=>"100",
                 "days_per_assignment"=>"22"
               },
             }
           }
    end
               
    @assn = assigns(:assignment)
    assert_redirected_to [@cs101, @assn.becomes(Assignment)]

    assert_not_equal @ts2.id, @assn.teamset_id
    assert_equal teamset_count + 1, Teamset.count
    assert_equal @ts2.active_teams.count, @assn.teamset.teams.count
  end

  test "should show assignment" do
    sign_in @john
    get :show, params: {id: @assn_no_teams_1, course_id: @cs101}
    assert_response :success
  end

  test "should get edit" do
    sign_in @fred
    get :edit, params: {id: @assn_no_teams_1, course_id: @cs101}
    assert_response :success
  end

  test "should update assignment without changing teamset" do
    sign_in @fred
    put :update, params: {
      id: @assn_no_teams_1, course_id: @cs101,
      assignment: { 
        teamset_plan: "use",
        teamset_source_use: @ts1,
        assignment: @assn_no_teams_1.assignment,
        name: "Something different",
        type: "Files",
      },
      lateness: {
        "type" => "lateness_UseCourseDefaultConfig",
      },
      graders: {
        "1477181088065"=> {
          "type"=>"1477181088065_ManualGrader",
          "id"=>"",
          "removed"=>"false",
          "JavaStyleGrader"=>{"avail_score"=>"50"},
          "CheckerGrader"=>{"avail_score"=>"50", "params"=>""},
          "JunitGrader"=>{"avail_score"=>"50", "params"=>""},
          "ManualGrader"=>{"avail_score"=>"50"}},
      }
    }
    assert_redirected_to [@cs101, assigns(:assignment).becomes(Assignment)]
  end

  ##################################################
  # Non-team assignment update tests
  ##################################################
  test "should update non-team assignment to new teamset" do
    sign_in @fred
    old_teamset = @assn_no_teams_1.teamset
    assert_equal 0, old_teamset.teams.count
    assert_difference('Submission.count') do
      sub = make_submission(@john, @assn_no_teams_1, "john.tar.gz")
    end
    assert_no_difference('Teamset.count') do
      put :update, params: {
            id: @assn_no_teams_1, course_id: @cs101,
            assignment: { 
              teamset_plan: "new",
              assignment: @assn_no_teams_1.assignment,
              name: "Something different",
              type: "Files",
            },
            lateness: {
              "type" => "lateness_UseCourseDefaultConfig",
            },
            graders: {
              "1477181088065"=> {
                "type"=>"1477181088065_ManualGrader",
                "id"=>"",
                "removed"=>"false",
                "JavaStyleGrader"=>{"avail_score"=>"50"},
                "CheckerGrader"=>{"avail_score"=>"50", "params"=>""},
                "JunitGrader"=>{"avail_score"=>"50", "params"=>""},
                "ManualGrader"=>{"avail_score"=>"50"}},
            }
          }
    end
    assert_redirected_to [@cs101, assigns(:assignment).becomes(Assignment)]
    @assn_no_teams_1.reload
    assert_equal 1, @assn_no_teams_1.teamset.teams.count
  end  
  
  test "should update non-team assignment to copy existing teamset" do
    sign_in @fred
    old_teamset = @assn_no_teams_1.teamset
    assert_equal 0, old_teamset.teams.count
    assert_difference('Submission.count') do
      sub = make_submission(@john, @assn_no_teams_1, "john.tar.gz")
    end
    assert_difference('Teamset.count') do
      put :update, params: {
            id: @assn_no_teams_1, course_id: @cs101,
            assignment: { 
              teamset_plan: "copy",
              teamset_source_copy: @ts2,
              assignment: @assn_no_teams_1.assignment,
              name: "Something different",
              type: "Files",
            },
            lateness: {
              "type" => "lateness_UseCourseDefaultConfig",
            },
            graders: {
              "1477181088065"=> {
                "type"=>"1477181088065_ManualGrader",
                "id"=>"",
                "removed"=>"false",
                "JavaStyleGrader"=>{"avail_score"=>"50"},
                "CheckerGrader"=>{"avail_score"=>"50", "params"=>""},
                "JunitGrader"=>{"avail_score"=>"50", "params"=>""},
                "ManualGrader"=>{"avail_score"=>"50"}},
            }
          }
    end
    assert_redirected_to [@cs101, assigns(:assignment).becomes(Assignment)]
    @assn_no_teams_1.reload
    assert_equal 1 + @ts2.active_teams.count, @assn_no_teams_1.teamset.teams.count
  end

  test "should not update non-team assignment to bogus teamset plan" do
    sign_in @fred
    assert_no_difference('Teamset.count') do
      put :update, params: {
            id: @assn_no_teams_1, course_id: @cs101,
            assignment: { 
              teamset_plan: "unique",
              assignment: @assn_no_teams_1.assignment,
              name: "Something different",
              type: "Files",
            },
            lateness: {
              "type" => "lateness_UseCourseDefaultConfig",
            },
            graders: {
              "1477181088065"=> {
                "type"=>"1477181088065_ManualGrader",
                "id"=>"",
                "removed"=>"false",
                "JavaStyleGrader"=>{"avail_score"=>"50"},
                "CheckerGrader"=>{"avail_score"=>"50", "params"=>""},
                "JunitGrader"=>{"avail_score"=>"50", "params"=>""},
                "ManualGrader"=>{"avail_score"=>"50"}},
            }
          }
    end
    assert_response 400
    assert_match "Impossible state: cannot unique", assigns(:assignment).errors.full_messages.join("\n")
  end
  
  test "should not update non-team assignment to blank teamset plan" do
    sign_in @fred
    assert_no_difference('Teamset.count') do
      put :update, params: {
            id: @assn_no_teams_1, course_id: @cs101,
            assignment: { 
              teamset_plan: "copy",
              teamset_source_copy: " ",
              assignment: @assn_no_teams_1.assignment,
              name: "Something different",
              type: "Files",
            },
            lateness: {
              "type" => "lateness_UseCourseDefaultConfig",
            },
            graders: {
              "1477181088065"=> {
                "type"=>"1477181088065_ManualGrader",
                "id"=>"",
                "removed"=>"false",
                "JavaStyleGrader"=>{"avail_score"=>"50"},
                "CheckerGrader"=>{"avail_score"=>"50", "params"=>""},
                "JunitGrader"=>{"avail_score"=>"50", "params"=>""},
                "ManualGrader"=>{"avail_score"=>"50"}},
            }
          }
    end
    assert_response 400
    assert_match "The teamset to be copied was not specified", assigns(:assignment).errors.full_messages.join("\n")
  end
  
  ##################################################
  # Team assignment update with no prior submissions
  ##################################################
  test "should allow using different teamset" do
    sign_in @fred
    old_teamset = @assn_teams_1.teamset
    assert_equal @cs101.students.count, old_teamset.teams.count
    assert_no_difference('Teamset.count') do
      put :update, params: {
            id: @assn_teams_1, course_id: @cs101,
            assignment: { 
              teamset_plan: "use",
              teamset_source_use: @ts1,
              assignment: @assn_teams_1.assignment,
              name: "Something different",
              type: "Files",
            },
            lateness: {
              "type" => "lateness_UseCourseDefaultConfig",
            },
            graders: {
              "1477181088065"=> {
                "type"=>"1477181088065_ManualGrader",
                "id"=>"",
                "removed"=>"false",
                "JavaStyleGrader"=>{"avail_score"=>"50"},
                "CheckerGrader"=>{"avail_score"=>"50", "params"=>""},
                "JunitGrader"=>{"avail_score"=>"50", "params"=>""},
                "ManualGrader"=>{"avail_score"=>"50"}},
            }
          }
    end
    assert_redirected_to [@cs101, assigns(:assignment).becomes(Assignment)]
    @assn_teams_1.reload
    assert_equal @ts1.id, @assn_teams_1.teamset_id
  end

  ##################################################
  # Team assignment update with existing submissions
  ##################################################
  test "should disallow using different teamset" do
    sign_in @fred
    old_teamset = @assn_teams_1.teamset
    assert_difference('Submission.count') do
      sub = make_submission(@john, @assn_teams_1, "hello.c")
    end
    assert_equal @cs101.students.count, old_teamset.teams.count
    assert_no_difference('Teamset.count') do
      put :update, params: {
            id: @assn_teams_1, course_id: @cs101,
            assignment: { 
              teamset_plan: "use",
              teamset_source_use: @ts1,
              assignment: @assn_teams_1.assignment,
              name: "Something different",
              type: "Files",
            },
            lateness: {
              "type" => "lateness_UseCourseDefaultConfig",
            },
            graders: {
              "1477181088065"=> {
                "type"=>"1477181088065_ManualGrader",
                "id"=>"",
                "removed"=>"false",
                "JavaStyleGrader"=>{"avail_score"=>"50"},
                "CheckerGrader"=>{"avail_score"=>"50", "params"=>""},
                "JunitGrader"=>{"avail_score"=>"50", "params"=>""},
                "ManualGrader"=>{"avail_score"=>"50"}},
            }
          }
    end
    assert_response 400
    assert_match "Impossible state: cannot use teamset because: Cannot reuse", assigns(:assignment).errors.full_messages.join("\n")
  end

  test "should allow cloning own shared teamset" do
    sign_in @fred
    old_teamset = @assn_shared_teams_1.teamset
    assert_equal @cs101.students.count, old_teamset.teams.count
    sub = assert_difference('Submission.count') do
      make_submission(@john, @assn_shared_teams_1, "hello.c")
    end
    assert_equal sub.team.teamset_id, old_teamset.id
    assert_difference('Teamset.count') do
      put :update, params: {
            id: @assn_shared_teams_1, course_id: @cs101,
            assignment: { 
              teamset_plan: "clone",
              assignment: @assn_shared_teams_1.assignment,
              name: "Something different",
              type: "Files",
            },
            lateness: {
              "type" => "lateness_UseCourseDefaultConfig",
            },
            graders: {
              "1477181088065"=> {
                "type"=>"1477181088065_ManualGrader",
                "id"=>"",
                "removed"=>"false",
                "JavaStyleGrader"=>{"avail_score"=>"50"},
                "CheckerGrader"=>{"avail_score"=>"50", "params"=>""},
                "JunitGrader"=>{"avail_score"=>"50", "params"=>""},
                "ManualGrader"=>{"avail_score"=>"50"}},
            }
          }
    end
    assert_redirected_to [@cs101, assigns(:assignment).becomes(Assignment)]
    sub.reload
    sub.team.reload
    assert_not_equal old_teamset.id, sub.team.teamset_id
  end
  
  test "should disallow cloning own unique teamset" do
    sign_in @fred
    old_teamset = @assn_teams_1.teamset
    assert_equal @cs101.students.count, old_teamset.teams.count
    sub = assert_difference('Submission.count') do
      make_submission(@john, @assn_teams_1, "hello.c")
    end
    assert_equal sub.team.teamset_id, old_teamset.id
    assert_no_difference('Teamset.count') do
      put :update, params: {
            id: @assn_teams_1, course_id: @cs101,
            assignment: { 
              teamset_plan: "clone",
              assignment: @assn_teams_1.assignment,
              name: "Something different",
              type: "Files",
            },
            lateness: {
              "type" => "lateness_UseCourseDefaultConfig",
            },
            graders: {
              "1477181088065"=> {
                "type"=>"1477181088065_ManualGrader",
                "id"=>"",
                "removed"=>"false",
                "JavaStyleGrader"=>{"avail_score"=>"50"},
                "CheckerGrader"=>{"avail_score"=>"50", "params"=>""},
                "JunitGrader"=>{"avail_score"=>"50", "params"=>""},
                "ManualGrader"=>{"avail_score"=>"50"}},
            }
          }
    end
    assert_response 400
    assert_match "Impossible state: cannot clone", assigns(:assignment).errors.full_messages.join("\n")
  end

  test "should destroy assignment" do
    sign_in @fred

    assert_difference('Assignment.count', -1) do
      delete :destroy, params: {id: @assn_shared_teams_1, course_id: @cs101}
    end

    assert_redirected_to @cs101
  end

  test "should allow for graders with files" do
    sign_in @fred
    upload_file = fixture_file_upload(
      "files/HelloSingle/hello.c",'application/octet-stream')
    assert_difference('Assignment.count') do
      post :create, params: {
             course_id: @cs101.id,
             assignment: {
               teamset_plan: "none",
               assignment: "Dance a jig.",
               points_available: 100,
               name: "Useful Work",
               due_date: '2019-05-22',
               available: '2011-05-22',
               type: "Files",
               lateness_config_attributes: {
                 "type"=>"LatePerHourConfig",
                 "frequency"=>"1",
                 "percent_off"=>"25",
                 "max_penalty"=>"100",
                 "days_per_assignment"=>"22"
               },
               graders_attributes: {
                 "1477181088065"=> {
                   "type"=>"JavaStyleGrader",
                   "avail_score"=>"50",
                   "order"=>"1",
                   "_destroy"=>"false",
                   "upload_file" => upload_file
                 }
               }
             }
           }
    end

    @assn = assigns(:assignment)
    assert_equal @assn, @assn.graders.first.upload.assignment
    assert_redirected_to [@cs101, @assn.becomes(Assignment)]
  end

end
