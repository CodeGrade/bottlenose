ENV["RAILS_ENV"] = "test"
require File.expand_path('../../config/environment', __FILE__)
require 'rails/test_help'
require 'capybara/rails'
require 'fake_upload'
require 'simplecov'
require 'backburner'
require 'beaneater'

SimpleCov.start

class ActionController::TestCase
  include Devise::Test::ControllerHelpers
end

class ActiveSupport::TestCase
  # Setup all fixtures in test/fixtures/*.(yml|csv) for all tests in alphabetical order.
  #
  # Note: You'll currently still have to declare fixtures explicitly in integration tests
  # -- they do not yet inherit this setting
  #fixtures :all

  # Add more helper methods to be used by all tests here...
  include FactoryGirl::Syntax::Methods

  def make_standard_course
    @ken      = create(:admin_user)
    @cs101    = create(:course, public: true)
    @ts1      = create(:teamset, course: @cs101, name: "Default teamset 1")
    @ts2      = create(:teamset, course: @cs101, name: "Default teamset 2")
    @ts3      = create(:teamset, course: @cs101, name: "Default teamset 3")
    @fred     = create(:user, name: "Fred McTeacher", first_name: "Fred", last_name: "McTeacher")
    @section  = create(:section, course: @cs101, instructor: @fred, crn: 12345)
    @fred_reg = create(:registration, course: @cs101, user: @fred,
                       role: Registration::roles[:professor], show_in_lists: false, new_sections: [@section.crn])
    @fred_reg.save_sections

    @students = []
    @regs     = []
    [["John", "Joker"], ["Sarah", "Studious"], ["Andy", "Assiduous"], ["Claire", "Crafter"]].each do |fn, ln|
      user = create(:user, name: "#{fn} #{ln}", first_name: fn, last_name: ln)
      @students.push(user)
      @regs.push(create(:registration, course: @cs101, user: user,
                        role: Registration::roles[:student], show_in_lists: true, new_sections: [@section.crn]))
      @regs.last.save_sections
    end
    @john     = @students[0]
    @sarah    = @students[1]
    @andy     = @students[2]
    @claire   = @students[3]
    @john_reg = @regs[0]
  end

  def simulated_upload(user, file)
    upload = build(:upload, user: user)
    upload.store_upload!(FakeUpload.new(file), {note: "Test: Simulated Upload"})
    upload
  end

  def assign_upload(assign, suffix)
    base = Rails.root.join('test', 'fixtures', 'files')
    ans = base.join(assign, "#{assign}-#{suffix}")
    if File.file? ans
      ans
    else
      base.join(assign, suffix)
    end
  end

  def assign_upload_obj(assign, suffix)
    fixture_file_upload(assign_upload(assign, suffix), 'application/octet-stream')
  end

  def make_assignment(bb, name)
    ts.save!
    aa = build(:assignment, bucket: bb, course: bb.course, name: name, teamset: ts1)
    aa.assignment_file = assign_upload_obj(name, 'assign.tar.gz')
    aa.grading_file    = assign_upload_obj(name, 'grading.tar.gz')
    aa.save_uploads!
    aa.save!

    aa
  end

  def make_submission(uu, aa, file)
    upl = build(:upload)
    upl.store_upload!(assign_upload_obj(aa.name, file),
                      {
                        type:       "Submission",
                        user:       "Test (0)",
                        date:       Time.now.strftime("%Y/%b/%d %H:%M:%S %Z")
                      })
    upl.save!

    sub = create(
      :submission,
      assignment: aa,
      type: "FilesSub",
      user: uu,
      upload_id: upl.id,
      team:
        if aa.team_subs?
          uu.active_team_for(aa.course, aa)
        else
          nil
        end
    )
    sub.save_upload
    sub.save!

    sub
  end

  def run_background_jobs
    bean = Beaneater.new("localhost:11300")
    conf = Backburner.configuration
    conf.max_job_retries = 0
    worker = conf.default_worker.new

    bean.tubes.each do |tube|
      next unless tube.name == 'bottlenose.test.backburner-jobs'

      while tube.peek(:ready)
        worker.prepare
        worker.work_one_job
      end
    end
  end
end

class ActiveRecord::ConnectionAdapters::PostgreSQLAdapter
  def supports_disable_referential_integrity?
    false
  end
end

DatabaseCleaner.strategy = :deletion
DatabaseCleaner.start
DatabaseCleaner.clean_with :truncation

Capybara.default_driver  = :rack_test

class ActionDispatch::IntegrationTest
  # Make the Capybara DSL available in all integration tests
  include Capybara::DSL

  # Stop ActiveRecord from wrapping tests in transactions
  self.use_transactional_tests = false

  setup do
    DatabaseCleaner.clean
  end

  teardown do
    Capybara.reset_sessions!    # Forget the (simulated) browser state
    Capybara.use_default_driver # Revert Capybara.current_driver to Capybara.default_driver

    DatabaseCleaner.clean
    Upload.cleanup_test_uploads!
  end

  def login_as(user)
    visit "/main/auth?email=#{user.email}&key=#{user.auth_key}"
    assert has_content?("Logged in as #{user.name}")
  end
end

Capybara::Webkit.configure do |config|
  config.allow_url("test.host")
end
