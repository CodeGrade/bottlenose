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

  test "should create code-review assignment" do
    sign_in @fred
    teamset_count = Teamset.count
    upload_file = fixture_file_upload(
      "peer-eval.yaml",'application/octet-stream')
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
               type: "Codereview",
               assignment_file: upload_file,
               graders_attributes: {
                 "1477181088065"=> {
                   "type"=>"CodereviewGrader",
                   "review_target"=>"self",
                   "review_count"=>"1",
                   "review_threshold"=>"75",
                   "order"=>"1",
                   "_destroy"=>"false",
                 }
               },
               related_assignment_id: @assn_no_teams_1.id,
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

  test "should create questions assignment" do
    sign_in @fred
    teamset_count = Teamset.count
    upload_file = fixture_file_upload(
      "questions.yaml",'application/octet-stream')
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
               type: "Questions",
               assignment_file: upload_file,
               graders_attributes: {
                 "1477181088065"=> {
                   "type"=>"QuestionsGrader",
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

  test "should create exam assignment" do
    sign_in @fred
    teamset_count = Teamset.count
    upload_file = fixture_file_upload(
      "test-exam.yaml",'application/octet-stream')
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
               type: "Exam",
               assignment_file: upload_file,
               graders_attributes: {
                 "1477181088065"=> {
                   "type"=>"ExamGrader",
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


  test "invalid questions should not create code-review assignment" do
    sign_in @fred
    teamset_count = Teamset.count
    upload_file = fixture_file_upload(
      "test-exam.yaml",'application/octet-stream')
    assert_no_difference('Assignment.count') do
      post :create, params: {
             course_id: @cs101.id,
             assignment: {
               teamset_plan: "none",
               assignment: "Dance a jig.",
               points_available: 100,
               name: "Useful Work",
               due_date: '2019-05-22',
               available: '2011-05-22',
               type: "Codereview",
               assignment_file: upload_file,
               graders_attributes: {
                 "1477181088065"=> {
                   "type"=>"CodereviewGrader",
                   "review_target"=>"self",
                   "review_count"=>"1",
                   "review_threshold"=>"75",
                   "order"=>"1",
                   "_destroy"=>"false",
                 }
               },
               related_assignment_id: @assn_no_teams_1.id,
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
  end

  test "invalid questions should not create questions assignment" do
    sign_in @fred
    teamset_count = Teamset.count
    upload_file = fixture_file_upload(
      "test-exam.yaml",'application/octet-stream')
    assert_no_difference('Assignment.count') do
      post :create, params: {
             course_id: @cs101.id,
             assignment: {
               teamset_plan: "none",
               assignment: "Dance a jig.",
               points_available: 100,
               name: "Useful Work",
               due_date: '2019-05-22',
               available: '2011-05-22',
               type: "Questions",
               assignment_file: upload_file,
               graders_attributes: {
                 "1477181088065"=> {
                   "type"=>"QuestionsGrader",
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
  end

  test "invalid questions should not create exam assignment" do
    sign_in @fred
    teamset_count = Teamset.count
    upload_file = fixture_file_upload(
      "peer-eval.yaml",'application/octet-stream')
    assert_no_difference('Assignment.count') do
      post :create, params: {
             course_id: @cs101.id,
             assignment: {
               teamset_plan: "none",
               assignment: "Dance a jig.",
               points_available: 100,
               name: "Useful Work",
               due_date: '2019-05-22',
               available: '2011-05-22',
               type: "Exam",
               assignment_file: upload_file,
               graders_attributes: {
                 "1477181088065"=> {
                   "type"=>"ExamGrader",
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
      "fundies-config.json",'application/octet-stream')
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
    upload = @assn.graders.first.upload
    assert_equal @assn, upload.assignment
    assert upload.upload_dir.to_s.starts_with?(Upload.base_upload_dir.join(@cs101.id.to_s, @assn.id.to_s).to_s)
    assert File.exist?(upload.submission_path)
    assert_redirected_to [@cs101, @assn.becomes(Assignment)]
  end
  
  ##################################################
  # Editing weights
  ##################################################
  test "should not break on editing no weights" do
    sign_in @fred
      post :update_weights, params: {
             course_id: @cs101.id,
            }
    assert_redirected_to [@cs101, :assignments]
  end

  ##################################################
  # SubmissionEnabledToggles and interlocks
  ##################################################
  test "should not allow two check_section_toggles interlocks on create" do
    sign_in(@fred)
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
        interlocks_attributes: {
          "1539721230721" => {
            "constraint"=>"check_section_toggles"
          },
          "1539721230722" => {
            "constraint"=>"check_section_toggles"
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
    assert_response 400
    @assn = assigns(:assignment)
    assert_equal(["Can only have one section-based interlock"], @assn.errors.full_messages)
  end

  test "creating an assignment with one check_section_toggles interlock works" do
    sign_in(@fred)
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
        interlocks_attributes: {
          "1539721230721" => {
            "constraint"=>"check_section_toggles"
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
    assert_response 302
    @assn = assigns(:assignment).becomes(Assignment)
    assert_redirected_to [@cs101, @assn]
  end

  test "should not allow two check_section_toggles interlocks on update" do
    @assn = create(:assignment, course: @cs101, teamset: @ts1)

    @lock = Interlock.new(
      constraint: "check_section_toggles",
      assignment: @assn,
      related_assignment: @assn
    )
    assert @lock.save

    sign_in(@fred)
    put :update, params: {
      id: @assn.id,
      course_id: @cs101.id,
      assignment: {
        interlocks_attributes: {
          "1539721230726" => {
            "constraint"=>"check_section_toggles"
          }
        },
      }
    }
    assert_response 400
    @assn = assigns(:assignment)
    assert_equal(["Can only have one section-based interlock"], @assn.errors.full_messages)
  end

  test "flip section toggles from assignment edit page" do
    @assn = create(:assignment, course: @cs101, teamset: @ts1)
    @lock = Interlock.new(
      constraint: "check_section_toggles",
      assignment: @assn,
      related_assignment: @assn
    )
    @lock.save
    @cs101.reload

    sign_in(@fred)
    get :show, params: {id: @assn.id, course_id: @cs101.id}

    sets = @lock.submission_enabled_toggles.to_a
    sets.each do |set|
      assert_not set.submissions_allowed
      patch :update_section_toggles, params: {
        assignment_id: @assn.id,
        course_id: @cs101.id,
        submission_enabled_toggle_id: set.id,
        state: true
      }, xhr: true
      set.reload
      assert set.submissions_allowed
    end
  end

  test "flip section toggles when they've already been flipped" do
    @assn = create(:assignment, course: @cs101, teamset: @ts1)
    @lock = Interlock.new(
      constraint: "check_section_toggles",
      assignment: @assn,
      related_assignment: @assn
    )
    @lock.save
    @cs101.reload

    sign_in(@fred)
    get :show, params: {id: @assn.id, course_id: @cs101.id}

    sets = @lock.submission_enabled_toggles.to_a
    sets.each do |set|
      assert_not set.submissions_allowed
      set.update_attribute(:submissions_allowed, true)
      assert set.submissions_allowed
      patch :update_section_toggles, params: {
        assignment_id: @assn.id,
        course_id: @cs101.id,
        submission_enabled_toggle_id: set.id,
        state: true
      }, xhr: true
      assert_response 409
      set.reload
      # this should not have changed the value, since it was true and the user requested true
      # it should just show the new values to the user
      assert set.submissions_allowed
    end
  end

  test "ensure only one interlock is created when assignment is updated" do
    @files = Files.new(course: @cs101, teamset: @ts1, team_subs: false, lateness_config: @cs101.lateness_config,
                       name: "Files hw", due_date: Date.current - 1.days + 12.hours, points_available: 10,
                       available: Date.current - 1.days, blame: @fred)
    @files.graders << JavaStyleGrader.new(order: 1, avail_score: 50, params: "")
    @files.graders << ManualGrader.new(order: 2, avail_score: 75, params: "")
    @files.save!

    @codereview = Codereview.new(course: @cs101, teamset: @ts1, team_subs: false,
                                 lateness_config: @cs101.lateness_config, related_assignment: @files,
                                 name: "Codereview hw",
                                 due_date: Date.current - 1.days + 12.hours, points_available: 10,
                                 available: Date.current - 1.days, blame: @fred)
    @codereview.assignment_file = assign_upload_obj("", "peer-eval.yaml")
    @codereview.graders << CodereviewGrader.new(avail_score: 5, order: 1,
                                                review_target: "self", review_count: 1, review_threshold: 75)
    @codereview.save!

    @lock = Interlock.new(
        constraint: "no_submission_after_viewing",
        assignment: @files,
        related_assignment: @codereview
    )
    @lock.save!

    sign_in(@fred)
    assert_equal 1, @codereview.related_interlocks.size
    assert_equal 1, Interlock.where(related_assignment: @codereview).size

    @codereview.update_attribute(:prevent_late_submissions, @files.id)
    @codereview.save!
    @codereview.reload

    assert_equal 1, @codereview.related_interlocks.size
    assert_equal 1, Interlock.where(related_assignment: @codereview).size
  end
end
