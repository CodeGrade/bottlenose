require 'active_record'
APP_PATH = File.expand_path('../../../config/application',  __FILE__)
require File.expand_path('../../../config/boot',  __FILE__)
require APP_PATH
Rails.env = ENV["RAILS_ENV"] = "test"
Rails.logger = ActiveSupport::Logger.new("/dev/null") # Don't need logging for this
Rails.application.require_environment!

require 'webdrivers'
require 'capybara'
require 'capybara/rails'
require 'fake_upload'
require 'backburner'
require 'beaneater'
require 'fileutils'
require 'database_cleaner'
require 'factory_bot'
require 'headless'
require 'open3'

module Utilities
  include Warden::Test::Helpers
  include Rails.application.routes.url_helpers
  def default_url_options
    ActionMailer::Base.default_url_options
  end

  def set_window_size(width = nil, height = nil)
    window = Capybara.current_session.current_window
    if width.nil? || height.nil?
      w, h = window.size()
      width = width || w
      height = height || h
    end
    window.resize_to(width, height)
  end
  def get_dpi
    page.evaluate_script("window.devicePixelRatio").to_f
  end
  def set_full_page
    height = page.evaluate_script("$(document.body).outerHeight(true)")
    set_window_size nil, height + @chrome_height
  end
  def sign_in(who)
    Warden.test_mode!
    Warden.test_reset!
    login_as(who, scope: :user)
  end
  def warden_scope(*resource)
    resource.class.name.underscore.to_sym
  end

  def highlight_area(id, box, strokeColor = "rgba(244, 208, 63, 0.5)", fillColor = "rgba(247, 220, 111, 0.5)")
    width = box["right"] - box["left"]
    height = box["bot"] - box["top"]
    page.evaluate_script(<<-JAVASCRIPT, id, box["left"], box["top"], width, height, fillColor, strokeColor)
(function(id, left, top, width, height, fillColor, strokeColor) {
  var canvas = document.createElement("canvas");
  canvas.width = width;
  canvas.height = height;
  canvas.id = "hilite_" + id;
  var $canvas = $(canvas);
  var ctx = canvas.getContext("2d");
  var n = 15;

  ctx.save();
    ctx.translate(width / 2, height / 2);
    ctx.beginPath();
    ctx.moveTo(width / 2, 0);
    var angle = Math.PI / n;
    for (var i = 0; i < n; i++) {
      var inX = Math.cos(angle * (2 * i + 1)) * (width / 2) * 0.9;
      var inY = Math.sin(angle * (2 * i + 1)) * (height / 2) * 0.9;
      var outX = Math.cos(angle * (2 * i + 2)) * (width / 2);
      var outY = Math.sin(angle * (2 * i + 2)) * (height / 2);
      ctx.lineTo(inX, inY);
      ctx.lineTo(outX, outY);
    }
    ctx.closePath(); 
    ctx.save();
      ctx.scale(width / 2, height / 2);
      var gradient = ctx.createRadialGradient(0, 0, 0, 0, 0, 1, 1);
      gradient.addColorStop(0, "rgba(255, 255, 255, 0)");
      gradient.addColorStop(1, fillColor);

      ctx.fillStyle = gradient;
      ctx.fill();
    ctx.restore();

    ctx.strokeStyle = strokeColor;
    ctx.lineWidth = 3;
    ctx.stroke();
  ctx.restore();

  $(document.body).append($canvas);
  $canvas.offset({left: left, top: top});
  return canvas;
}).apply(this, arguments)
JAVASCRIPT
  end
  def remove_highlight(id)
    page.execute_script("$('#hilite_" + id + "').remove()")
  end

  
  def bbox(*elems)
    elems = elems.map do |e|
      if e.is_a? Capybara::Result
        e.to_a
      else
        [e]
      end
    end.flatten
    page.evaluate_script(<<-JAVASCRIPT, *elems)
(function() {
  var dimen = {
    top: Number.MAX_VALUE, left: Number.MAX_VALUE, 
    right: -Number.MAX_VALUE, bot: -Number.MAX_VALUE
  };
  for (var i = 0; i < arguments.length; i++) {
    var $e = $(arguments[i]);
    var off = $e.offset();
    dimen.left  = Math.min(off.left, dimen.left);
    dimen.top   = Math.min(off.top,  dimen.top);
    dimen.right = Math.max(off.left + $e.outerWidth(), dimen.right);
    dimen.bot   = Math.max(off.top + $e.outerHeight(), dimen.bot);
  }
  return dimen;
}).apply(this, arguments)
JAVASCRIPT
  end

  def clip(box, width, height)
    if width
      if width > 0
        box["right"] = [box["left"] + width, box["right"]].min
      else
        box["left"] = [box["right"] - width, box["left"]].max
      end
    end
    if height
      if height > 0
        box["bot"] = [box["top"] + height, box["bot"]].min
      else
        box["top"] = [box["bot"] - height, box["top"]].max
      end
    end
    box
  end

  def scale_box(box, scaleX, scaleY)
    box["left"] *= scaleX
    box["top"] *= scaleY
    box["width"] *= scaleX if box["width"]
    box["height"] *= scaleY if box["height"]
    box["right"] *= scaleX if box["right"]
    box["bot"] *= scaleY if box["bot"]
  end
  
  def inflate_box_pct(box, marginPctX, marginPctY)
    width = box["width"] || (box["right"] - box["left"])
    height = box["height"] || (box["bot"] - box["top"])
    dx = width * marginPctX
    dy = height * marginPctY
    box["left"] -= dx;
    box["top"] -= dy
    box["right"] += dx if box["right"]
    box["bot"] += dy if box["bot"]
    box["width"] += 2 * dx if box["width"]
    box["height"] += 2 * dy if box["height"]
    box
  end

  def inflate_box(box, left, top = nil, right = nil, bottom = nil)
    top = top || left
    right = right || left
    bottom = bottom || top
    box["left"] -= left
    box["top"] -= top
    box["right"] += right
    box["bot"] += bottom
    box["width"] += (left + right) if box["width"]
    box["height"] += (top + bottom) if box["height"]
    box
  end

  MALE_FIRST_NAMES = [
    "Santiago", "Daniel", "Miguel", "Noah",
    "William", "Agustin", "Jayden", "Luis",
    "Mohammed", "Ori", "Marc", "Stefan",
    "Jakub", "Peter", "Bo", "Wei", "Peng"
  ]
  FEMALE_FIRST_NAMES = [
    "Sofia", "Alice", "Olivia", "Emma",
    "Chloe", "Tamar", "Eden", "Emily",
    "Ma'ayan", "Sara", "Jessica", "Sophia",
    "Fatima", "Mariam", "Ting", "Qian", "Jing"
  ]
  LAST_NAMES = [
    "Baek", "Brown", "Chen", "Davis",
    "Diaz", "Edwards", "Evans", "Gagnon",
    "Garcia", "Gonzalez", "Hernandez", "Johnson",
    "Jones", "Li", "Liu", "Magoro",
    "Martinez", "Miller", "Munoz", "Murphy",
    "Nguyen", "Okafor", "Parker", "Robinson",
    "Rodriguez", "Silva", "Smith", "Suzuki",
    "Taylor", "Wang", "Williams", "Wilson", "Zhang"
  ]
  MALE_PROFILES = Dir[File.expand_path("demo-images/man_*", File.dirname(__FILE__))]
  FEMALE_PROFILES = Dir[File.expand_path("demo-images/woman_*", File.dirname(__FILE__))]

  RANDOM_PEOPLE = []
  FileUtils.mkdir_p Upload.base_upload_dir unless Dir.exists? Upload.base_upload_dir
  [MALE_PROFILES, FEMALE_PROFILES].each do |profs|
    profs.each do |prof|
      FileUtils.cp(prof, Upload.base_upload_dir)
      prof.gsub!(File.dirname(prof).to_s, Upload.base_upload_dir.to_s)
    end
  end
    
  LAST_NAMES.each do |ln|
    [[MALE_FIRST_NAMES, MALE_PROFILES], [FEMALE_FIRST_NAMES, FEMALE_PROFILES]].each do |fns, profs|
      fns.each do |fn|
        prof = profs.sample
        RANDOM_PEOPLE << [fn, ln, prof]
      end
    end
  end
  def self.profile(male)
    if male
      prof = MALE_PROFILES.sample
    else
      prof = FEMALE_PROFILES.sample
    end
  end
  def profile(male)
    Utilities.profile(male)
  end
  def self.redefine_factories
    FactoryBot.define do
      sequence :first_name do |n|
        RANDOM_PEOPLE[n % RANDOM_PEOPLE.count][0]
      end
      sequence :last_name do |n|
        RANDOM_PEOPLE[n % RANDOM_PEOPLE.count][1]
      end
      sequence :profile do |n|
        RANDOM_PEOPLE[n % RANDOM_PEOPLE.count][2]
      end
    end
    FactoryBot.modify do
      factory :user do
        first_name { generate(:first_name) }
        last_name { generate(:last_name) }
        profile { generate(:profile) }
        name { "#{first_name} #{last_name}" }
        email { username + "@school.edu" }
        sequence :nuid do |n| n.hash % 1e9.to_i end
      end
      factory :section do
        sequence :crn do |n| n.hash % 90000 + 10000 end
        meeting_time {
          num_days = [1,2,3].sample
          days = [0, 1, 2, 3, 4].sample(num_days).sort
          day = "MTWRF".chars.values_at(*days).join("")

          times = ["9:15--10:20am", "9:50--11:30am", "10:35--11:40am", "1:35--3:15pm"].sample
          "#{day} #{times}"
        }
      end
    end
  end
end

class Screenshots
  include Capybara::DSL
  include FactoryBot::Syntax::Methods
  include ActionDispatch::TestProcess
  include Utilities

  BROWSER_WIDTH  = 1280
  BROWSER_HEIGHT = 768

  HOOKS = {before_all: [], before_each: [], after_all: [], after_each: []}

  def self.before_all(method)
    HOOKS[:before_all] << method
  end
  def self.before_each(method)
    HOOKS[:before_each] << method
  end
  def self.after_all(method)
    HOOKS[:after_all] << method
  end
  def self.after_each(method)
    HOOKS[:after_each] << method
  end

  def self.generate(path, width: BROWSER_WIDTH, height: BROWSER_HEIGHT)
    Capybara.save_path = path
    Headless.ly do
      DatabaseCleaner.clean_with :truncation
      
      ss = Screenshots.new
      ss.extend(ScreenshotScripts)
      # Set desired dimensions
      ss.width = width
      ss.height = height
      # And add the fudge factor of the window chrome
      ss.chrome_height =
        Capybara.current_session.current_window.size()[1] - ss.page.evaluate_script("window.innerHeight")

      dpi = ss.get_dpi
      puts "DPI is #{dpi}"
      
      HOOKS[:before_all].each do |m| ss.send(m) end
      ScreenshotScripts.instance_methods(false).each do |method|
        HOOKS[:before_each].each do |m| ss.send(m) end
        @count = 0
        @default_filename = method.to_s.gsub("_", "-")
        print "Running script #{method}..."
        ss.send(method) do |file = @default_filename, options: {}|
          if file == @default_filename
            filename = "#{file}_#{@count}.png"
            @count += 1
          else
            filename = "#{file}.png"
          end
          ss.page.save_screenshot(filename)
          if options["left"] && options["top"]
            options["left"] = options["left"].floor
            options["top"] = options["top"].floor
            if options["right"] && options["bot"]
              options["width"] = options["right"].ceil - options["left"]
              options["height"] = options["bot"].ceil - options["top"]
            end
            if options["width"] && options["height"]
              ss.scale_box options, dpi, dpi
              geometry = "#{options["width"].ceil}x#{options["height"].ceil}+#{options["left"]}+#{options["top"]}"
              output, err, status =
                           Open3.capture3("convert",
                                          path.join(filename).to_s,
                                          "-crop",
                                          geometry,
                                          path.join(filename).to_s)
              if !status.success?
                puts output
                puts err
                puts status
              end
            end
          end
        end
        puts "Done."
        HOOKS[:after_each].each do |m| ss.send(m) end
      end
      HOOKS[:after_all].each do |m| ss.send(m) end
      
      dir = Upload.base_upload_dir.to_s
      if Rails.env.test? && dir =~ /test/
        puts "Cleaning up #{dir}"
        FileUtils.rm_rf(dir)
      end
    end
  end

  def self.optimize(path)
    Headless.ly do
      Dir["#{path}/**"].sort.each do |filename|
        puts "Optimizing #{filename}"
        `pngquant --force --output #{filename} #{filename}`
      end
    end
  end

  #######################################################

  def reset_db
    DatabaseCleaner.clean
  end
  
  def create_students
    print "Creating #{RANDOM_PEOPLE.count} students..."
    User.transaction do
      @students = RANDOM_PEOPLE.map do |fn, ln, profile|
        User.create(first_name: fn, last_name: ln, name: "#{fn} #{ln}", profile: profile)
      end
    end
    puts "Done."
  end
  
  def create_course
    print "Creating default course..."
    @fred = create(:user, name: "Fred McTeacher", first_name: "Fred", last_name: "McTeacher",
                   nickname: "Fred", profile: profile(true))
    @term = Term.create(semester: Term.semesters[:fall], year: Date.today.year, archived: false)
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

    @students.sample(60).each do |student|
      reg = Registration.create(course: @course, user: student,
                                role: Registration::roles[:student], show_in_lists: true,
                                new_sections: [@sections.sample])
      reg.save_sections
    end
    puts "Done."

    @assignments = []
    # ASSIGNMENT 1 -- Fully graded, published
    print "Creating Assignment 1..."
    ts = Teamset.create(course: @course, name: "Teamset 1")
    assn = Files.create(name: "Assignment 1", blame: @fred, teamset: ts, lateness_config: @late_per_day,
                        course: @course, available: Time.now - 15.days, due_date: Time.now - 10.days,
                        points_available: 2.5)
    assn.graders << RacketStyleGrader.new(assignment: assn, params: "80", avail_score: 30, order: 1)
    assn.graders << ManualGrader.new(assignment: assn, avail_score: 50, order: 2)
    assn.save!
    @assignments << assn
    # These take a while, because they each launch DrRacket to render the file...
    # This assignment is fully graded, and published
    create_submission(@course, @course.students, assn, "sample.rkt", 15)
      .each do |s| grade_sub(s, assn.graders.to_a, true) end
    puts "Done."


    # ASSIGNMENT 2 -- Full graded, unpublished
    print "Creating Assignment 2..."
    ts = Teamset.create(course: @course, name: "Teamset 2")
    ts.randomize(2, "course", Time.now - 10.days)
    assn = Files.create(name: "Assignment 2", blame: @fred, teamset: ts, lateness_config: @late_per_day,
                        course: @course, available: Time.now - 10.days, due_date: Time.now - 5.days,
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
    create_submission(@course, @course.students, assn, "Mobiles.java", 15)
    # This assignment is fully graded, but unpublished
    assn.submissions.each do |s| grade_sub(s, assn.graders.to_a, false) end
    puts "Done."
    
    
    # ASSIGNMENT 3
    print "Creating Assignment 3..."
    ts = Teamset.create(course: @course, name: "Teamset 3")
    ts.randomize(2, "course", Time.now - 10.days)
    assn = Files.create(name: "Assignment 3", blame: @fred, teamset: ts, lateness_config: @late_per_day,
                        course: @course, available: Time.now - 10.days, due_date: Time.now - 5.days,
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
    create_submission(@course, @course.students, assn, "Mobiles.java", 15)
    # This assignment is fully ungraded, and no grades even exist yet
    # This assignment should also have an abnormal submission for the team below
    assn.used_submissions[0].team.dissolve(assn.due_date - 1.day)
    puts "Done."


    # ASSIGNMENT 4
    print "Creating Assignment 4..."
    ts = Teamset.create(course: @course, name: "Teamset 4")
    ts.randomize(3, "course", Time.now - 10.days)
    assn = Files.create(name: "Assignment 4", blame: @fred, teamset: ts, lateness_config: @late_per_day,
                        course: @course, available: Time.now - 5.days, due_date: Time.now - 1.days,
                        points_available: 2.5, team_subs: true)
    u = build(:upload, user: @fred, assignment: assn)
    u.upload_data = FakeUpload.new(Rails.root.join("test", "fixtures", "files", "fundies-config.json").to_s)
    assn.graders << JavaStyleGrader.new(assignment: assn, avail_score: 30, order: 1, upload: u)
    assn.graders << ManualGrader.new(assignment: assn, avail_score: 50, order: 3)
    assn.save!
    @assignments << assn
    create_submission(@course, @course.students, assn, "Mobiles.java", 15)
    # This assignment has grades filled in for the first two graders, but not the third yet,
    # so it is missing
    graders = assn.graders.to_a.sort_by(&:order)
    assn.submissions.each do |s| grade_sub(s, graders[0...-1], false) end
    puts "Done."
    

    # ASSIGNMENT 1 Self-eval
    print "Creating Assigmment 1 Self-eval..."
    assn1 = @assignments[0]
    u = build(:upload, user: @fred, assignment: assn1)
    u = FakeUpload.new(Rails.root.join("test", "fixtures", "files", "peer-eval.yaml").to_s)
    assn = Codereview.new(name: "Assignment 1 self-eval", blame: @fred, teamset: assn1.teamset,
                          lateness_config: @late_per_day, course: @course, related_assignment: assn1,
                          available: assn1.due_date, due_date: assn1.due_date + 25.hours,
                          points_available: 2.5, team_subs: assn1.team_subs, assignment_file: u,
                          prevent_late_submissions: assn1.id)
    assn.graders << CodereviewGrader.new(review_target: "self", review_count: 1, review_threshold: 75,
                                         upload_by_user_id: @fred.id, order: 1)
    assn.save!
    @assignments << assn
    puts "Done."

    # ASSIGNMENT 2 Peer-eval
    print "Creating Assigmment 2 Self-eval..."
    assn2 = @assignments[1]
    u = FakeUpload.new(Rails.root.join("test", "fixtures", "files", "peer-eval.yaml").to_s)
    assn = Codereview.new(name: "Assignment 2 self-eval", blame: @fred, teamset: assn2.teamset,
                          lateness_config: @late_per_day, course: @course, related_assignment: assn2,
                          available: assn2.due_date, due_date: assn2.due_date + 25.hours,
                          points_available: 2.5, team_subs: assn2.team_subs, assignment_file: u,
                          prevent_late_submissions: assn2.id)
    assn.graders << CodereviewGrader.new(review_target: "peer", review_count: 2, review_threshold: 75,
                                         upload_by_user_id: @fred.id, order: 1)
    assn.save!
    @assignments << assn
    puts "Done."
    
    # ASSIGNMENTS 5--7
    (5..7).each do |i|
      print "Creating Assignment #{i}..."
      ts = Teamset.create(course: @course, name: "Teamset #{i}")
      assn = Files.create(name: "Assignment #{i}", blame: @fred, teamset: ts, lateness_config: @late_per_day,
                          course: @course, available: Time.now + 1.days, due_date: Time.now + 7.days,
                          points_available: 2.5, team_subs: false)
      assn.graders << ManualGrader.new(assignment: assn, avail_score: 50, order: 1)
      assn.save!
      @assignments << assn
      puts "Done."
    end
  end

  def create_submission(course, students, assn, file, count = 1)
    students.to_a.sample(count).map do |user|
      sub = FilesSub.create(assignment: assn, user: user,
                            team: user.active_team_for(course, assn), created_at: assn.available + 1.5.days)
      sub.upload_file = fixture_file_upload(Rails.root.join("test", "fixtures", "files/#{assn.name}/#{file}"),
                                            "application/octet-stream")
      sub.save_upload
      sub.save!
      sub.set_used_sub!
      sub
    end
  end

  def grade_sub(sub, graders, complete)
    graders.each do |g| g.ensure_grade_exists_for! sub end
    sub.grades.each do |g|
      g.score = Random.rand(g.out_of)
      g.available = g.grader.autograde? || complete
      g.save
    end
    sub.compute_grade! if complete
  end
  
  def set_default_size
    set_window_size(@width, @height + @chrome_height)
  end

  attr_accessor :chrome_height
  attr_accessor :width
  attr_accessor :height

  before_all :reset_db
  before_all :create_students
  before_all :create_course
  before_each :set_default_size
  after_all :reset_db

  module ScreenshotScripts
    def login
      logout(:user)
      visit("/")
      yield
      yield options: bbox(page.find_all("input.form-control"), page.find_all("label"))
    end
    def homepage
      sign_in(@fred)
      visit("/")
      yield
    end

    def initial_profile
      sign_in(@fred)
      visit(user_path(@fred))
      yield
      visit(edit_user_path(@fred))
      yield
      find("input.btn[name='commit']").click
      yield
    end
    def course_page
      sign_in(@fred)
      
      visit(course_path(@course))
      yield
      assignments, missing, abnormal, unpublished, current_teams, pending = page.find_all("div.panel").to_a
      set_full_page
      yield options: bbox(missing)
      yield options: bbox(abnormal)
      # Fill in the missing graders, and now it should be pending
      grader = @assignments[3].graders.last
      @assignments[3].submissions.each do |s|
        @assignments[3].graders.each do |g|
          g.ensure_grade_exists_for! s
        end
      end
      visit(course_path(@course))
      assignments, abnormal, unpublished, current_teams, pending = page.find_all("div.panel").to_a
      yield options: bbox(unpublished)
      yield options: bbox(pending)
      set_default_size
      box = bbox(page.find_all("a.btn").to_a[2])
      inflate_box_pct box, 0.1, 0.1
      highlight_area("assignments", box)
      yield
    end
    def assignments_page
      sign_in(@fred)
      visit(course_assignments_path(@course))
      #page.click_link("Assignments") # was used when we were on course_path(@course)
      yield
      set_full_page
      page.find_all(".panel").each do |panel|
        yield options: clip(bbox(panel), nil, 400)
      end
      summary = @course.score_summary
      student = summary.sort_by{|s| s[:used].size}.last
      row = page.find("tr", text: student[:s].display_name)
      row.click
      yield options: bbox(row)
    end

    def facebook_page
      sign_in(@fred)
      visit facebook_course_path(@course)
      yield
    end

    def assignment_page
      sign_in(@fred)
      # Fully graded & published assignment
      visit course_assignment_path(@course, @assignments[0])
      yield

      # Fully graded, unpublished assignment
      visit(course_assignment_path(@course, @assignments[1]))
      highlight_area("publish", inflate_box_pct(bbox(page.find("a[data-confirm]")), 0.1, 0.1))
      yield
      remove_highlight("publish")
      accept_alert do
        page.find("a[data-confirm]").click
      end
      yield # fully graded and published

      # puts Grade.count
      subs = @assignments[2].submissions
      graders = @assignments[2].graders.to_a.sort_by(&:order)
      last_grader = graders.pop
      subs.each do |s|
        graders.each do |g|
          g.ensure_grade_exists_for! s
        end
        s.grades.each do |g|
          g.score = Random.rand(g.out_of)
          g.save
        end
      end
      visit course_assignment_path(@course, @assignments[2])
      create_missing = page.find("a.btn-warning")
      highlight_area("assignments", inflate_box_pct(bbox(create_missing), 0.1, 0.1), "rgba(240, 0, 0, 0.5)")
      yield # Mostly graded assignment
      remove_highlight("assignments")
      accept_alert do
        create_missing.click
      end
      yield
      visit course_assignment_path(@course, @assignments[3])
      yield

      histograms = page.find("button#toggle-histograms")
      actions = histograms.find(:xpath, "..")
      histograms_hilite = highlight_area("histograms", inflate_box_pct(bbox(histograms), 0.1, 0.1), "rgba(240, 0, 0, 0.5)")
      yield options: bbox(actions, histograms_hilite) # Mostly graded assignment
      remove_highlight("histograms")
      histograms.click

      grades = page.find("div#grade-histograms")
      left = grades.find(".left.carousel-control")
      right = grades.find(".right.carousel-control")
      highlight_area("left", bbox(left.find(".glyphicon")))
      highlight_area("right", bbox(right.find(".glyphicon")))
      yield options: bbox(grades, left, right)
      remove_highlight("left")
      remove_highlight("right")
      right.click
      yield options: bbox(grades, left, right)
      right.click
      yield options: bbox(grades, left, right)
    end

    def assignment_extensions_page
      visit course_assignment_path(@course, @assignments[3])
      extensions = page.find("a", text: "Manage individual extensions")
      highlight_area("extend", inflate_box_pct(bbox(extensions), 0.1, 0.1))
      yield
      remove_highlight("extend")
      extensions.click
      yield # extensions page

      row = page.find_all("tbody tr").first
      text = row.find("input#due_date")
      text.click
      widget = row.find(".bootstrap-datetimepicker-widget")
      yield options: bbox(row, text, widget)
      submit = row.find(".submit")
      text.set(DateTime.now + 2.days)
      submit.click
      yield options: bbox(row)

      page.refresh
      hilite = highlight_area("revoke_all", inflate_box_pct(bbox(page.find("a#revoke_all")), 0.1, 0.1))
      yield options: bbox(page.find("h3", text: "Existing extensions"), hilite)
      remove_highlight("revoke_all")

      row = page.find_all("tbody tr").first
      row.find(".revoke").click
      yield options: bbox(row)
    end

    def assignment_weights_page
      sign_in(@fred)
      visit weights_course_assignments_path(@course)
      yield
      find(:css, "input#weight_#{@assignments[2].id}").set("97").send_keys(:tab, [:shift, :tab])
      page.execute_script("window.scrollBy(0, 10000)"); # scroll to bottom
      yield
    end

    def review_matchings_page
      sign_in(@fred)
      visit course_assignment_path(@course, @assignments[5])
      matchings_button = page.find("a", text: "Edit review matchings")
      hilite = highlight_area("matchings", inflate_box_pct(bbox(matchings_button), 0.1, 0.1))
      info = matchings_button.find_all(:xpath, "../following-sibling::div[@class='row']")
      yield options: bbox(info, hilite)
      remove_highlight("matchings")
      page.find("a", text: "Edit review matchings").click
      page.find_all("input[value='Assign review matchings']")[0].click
      set_full_page
      well = page.find_all("div.well > *")
      yield options: inflate_box(bbox(well[0..1]), 19, 19, 0)
      yield options: inflate_box(bbox(well[2..5]), 19)
      yield options: inflate_box(bbox(well[6..8]), 19)
      yield options: clip(bbox(page.find_all("div.well ~ *")), nil, 300)
    end
  end
end

if ENV["SEED"]
  RANDOM_SEED = ENV["SEED"].to_i
else
  srand
  RANDOM_SEED = srand % 0xFFFF
end
srand RANDOM_SEED
puts "SEED=#{RANDOM_SEED}"


FactoryBot.find_definitions
Utilities.redefine_factories
Capybara.default_driver = :selenium_chrome_headless
DatabaseCleaner.strategy = :deletion
DatabaseCleaner.start

path = Pathname.new(ARGV[1] || "./screenshots")
puts path
FileUtils.mkdir_p path unless File.directory? path
Screenshots.generate path
Screenshots.optimize path
