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
  canvas.style.zIndex = 1000;
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

  def random_number_of_length(len)
    base = 10 ** (len - 1)
    base += Random.rand(base * 10 - base)
    base
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

  def create_courses
    print "Creating default courses..."
    @fred = create(:user, name: "Fred McTeacher", first_name: "Fred", last_name: "McTeacher",
                   nickname: "Fred", profile: profile(true), username: "fred")
    @george = create(:user, name: "George McGrader", first_name: "George", last_name: "McGrader",
                     nickname: "George", profile: profile(true), username: "george")
    @henry = create(:user, name: "Henry McAssistant", first_name: "Henry", last_name: "McAssistant",
                    nickname: "Henry", profile: profile(true), username: "henry")
    @brian = create(:user, name: "Brian Johnson", first_name: "Brian", last_name: "Johnson",
                    nickname: "Brian", profile: profile(true), username: "brian")
    @chris = create(:user, name: "Chris Franklin", first_name: "Chris", last_name: "Franklin",
                    nickname: "Chris", profile: profile(true), username: "chris")
    @joe = create(:user, name: "Joe Jones", first_name: "Joe", last_name: "Jones",
                  nickname: "Joe", profile: profile(true), username: "joe")
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
    @fred_reg.save_sections
    @george_reg = Registration.create(course: @course, user: @george, role: Registration::roles[:grader],
                                      show_in_lists: false, new_sections: @sections)
    @george_reg.save_sections
    @henry_reg = Registration.create(course: @course, user: @henry, role: Registration::roles[:assistant],
                                     show_in_lists: false, new_sections: @sections)
    @henry_reg.save_sections
    @brian_reg = Registration.create(course: @course, user: @brian, role: Registration::roles[:grader],
                                     show_in_lists: false, new_sections: @sections)
    @brian_reg.save_sections
    @chris_reg = Registration.create(course: @course, user: @chris, role: Registration::roles[:grader],
                                     show_in_lists: false, new_sections: @sections)
    @chris_reg.save_sections
    @joe_reg = Registration.create(course: @course, user: @joe, role: Registration::roles[:grader],
                                   show_in_lists: false, new_sections: @sections)
    @joe_reg.save_sections
    @students.sample(60).each do |student|
      reg = Registration.create(course: @course, user: student,
                                role: Registration::roles[:student], show_in_lists: true,
                                new_sections: [@sections.sample])
      reg.save_sections
    end

    @alice = create(:user, name: "Alice Andrews", first_name: "Alice", last_name: "Andrews",
                    nickname: "Alice", profile: profile(true), username: "alice")
    @course2 = Course.new(name: "Fundamentals of Computer Science 1", term: @term, lateness_config: @late_per_day)
    @sections2 = (1..2).map do |_|
      build(:section, course: @course2, instructor: @alice)
    end
    @course2.sections = @sections2
    @course2.save
    @alice_reg = Registration.create(course: @course2, user: @alice, role: Registration::roles[:professor],
                                     show_in_lists: false, new_sections: @sections2)
    @alice_reg.save_sections

    @course3 = Course.new(name: "Fundamentals of Computer Science 2", term: @term, lateness_config: @late_per_day)
    @sections3 = (1..2).map do |_|
      build(:section, course: @course3, instructor: @alice)
    end
    @course3.sections = @sections3
    @course3.save
    @alice_reg2 = Registration.create(course: @course3, user: @alice, role: Registration::roles[:professor],
                                     show_in_lists: false, new_sections: @sections3)
    @alice_reg2.save_sections
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

    print "Creating Assignment Test..."
    ts = Teamset.create(course: @course, name: "Teamset 8")
    assn = Files.create(name: "Assignment Test", blame: @fred, teamset: ts, lateness_config: @late_per_day,
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
  before_all :create_courses
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
      courses_menu = page.find("li.dropdown")
      bb = bbox(courses_menu)
      navbar = page.find("div.navbar")
      yield options: bbox(navbar)
      hilite = highlight_area("courses", bb)
      yield options: bbox(navbar, hilite)
      remove_highlight "courses"
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

    def new_course_page
      sign_in(@fred)
      visit(courses_path())
      course_container = page.find_all("div.container")[2]
      course_bbox = bbox(course_container)
      yield options: course_bbox

      find(:xpath, ".//a[text()='New Course']").click
      page.find("#course_name").set("CS3500 - Object-Oriented Design")
      name = page.find(:xpath, ".//div[./input[@id='course_name']]")
      term = page.find(:xpath, ".//div[./select[@id='course_term_id']]")
      name_term_bbox = bbox(name, term)
      yield options: inflate_box(name_term_bbox, 5, 5, 5, 5)

      page.find("a.add_fields").click
      page.find_all("input[id^='course_sections_attributes_'][id$='_crn']").each do |el|
        el.set(random_number_of_length(5))
      end
      page.find_all("input[id^='course_sections_attributes_'][id$='_prof_name']").each_with_index do |el, i|
        if i == 0
          el.set(@fred.username)
        else
          el.set(@alice.username)
        end
      end
      page.find_all("input[id^='course_sections_attributes_'][id$='_meeting_time']").each_with_index do |el, i|
        if i == 0
          el.set("MWR 10:30-11:35am (CG 094)")
        else
          el.set("T 9:50-11:30am (WVH 210)")
        end
      end
      page.find_all("select[id^='course_sections_attributes_'][id$='_type']").each_with_index do |el, i|
        if i == 1
          el.find("option[value='lab']").select_option
        end
      end
      sections = page.find(:xpath, ".//div[./table[@id='sections']]")
      sections_bbox = bbox(sections)
      yield options: inflate_box(sections_bbox, 5, 5, 5, 5)

      late_penalty = page.find(:xpath, ".//div[./div[@data-init-tab='LatePerDayConfig']]")
      late_penalty_bbox = bbox(late_penalty)
      yield options: inflate_box(late_penalty_bbox, 5, 5, 5, 5)

      set_full_page
      course_wide_footer = page.find(:xpath, ".//div[textarea[@id='course_footer']]")
      max_submission_size = page.find(:xpath, ".//div[label[@for='course_sub_max_size']]")
      public_access = page.find(:xpath, ".//div[input[@id='course_public']]")
      create_course = page.find(:xpath, ".//div[input[@value='Create Course']]")
      create_course_box = bbox(course_wide_footer, max_submission_size, public_access, create_course)
      yield options: inflate_box(create_course_box, 10, 10, 10, 10)
      set_default_size
    end

    def assignments_page
      sign_in(@fred)
      visit(course_assignments_path(@course))
      #page.click_link("Assignments") # was used when we were on course_path(@course)
      yield

      navbar = page.find("div.navbar")
      export = page.find(:xpath, ".//div[a[text()='Export... ']]")
      export_link = export.find("a")
      export_link.click
      export_items = export.find("ul")
      yield options: inflate_box(bbox(navbar, export_items), 0, 0, 0, 10)
      export_link.click

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

    def new_assignment_page
      sign_in(@fred)
      visit(course_assignments_path(@course))

      page.find(:xpath, ".//a[text()='New']").click
      yield
    end

    def new_file_assignment_page
      sign_in(@fred)
      visit(course_assignments_path(@course))
      set_full_page

      page.find(:xpath, ".//a[text()='New']").click
      file_div = page.find(:xpath, ".//div[@id='type-files']")
      name = file_div.find(:xpath, ".//input[@id='assignment_name']")
      name.set("Assignment 10")
      assignment_text = file_div.find(:xpath, ".//textarea[@id='assignment_assignment']")
      assignment_text.set("<a href=\"https://course.ccs.neu.edu/cs2500/ps1.html\">Assignment 1</a>: Please submit <b>ONE</b> .rkt file with all your work for this assignment")
      title = page.find(:xpath, ".//strong[text()='Create New Assignment']")
      yield options: bbox(name, title, assignment_text)

      file_upload = file_div.find(:xpath, ".//input[@id='assignment_assignment_file']", :visible => false)
      file_upload.set(File.expand_path('../../../test/fixtures/files/Assignment 1/sample.rkt',  __FILE__))
      file_upload_div = file_div.find(:xpath, ".//div[./p[@id='files']]")
      yield options: inflate_box(bbox(file_upload_div), 5, 5, 5, 5)

      file_div.find(:xpath, ".//input[@id='assignment_points_available']").set("0.5")
      points_available = file_div.find(:xpath, ".//div[label[@for='assignment_points_available']]")
      yield options: inflate_box(bbox(points_available), 5, 5, 5, 5)

      file_div.find(:xpath, ".//label[text()='Regular']").click
      # sleep so the toggle will complete
      sleep(0.5)
      yield options: inflate_box(bbox(points_available), 5, 5, 5, 5)

      due_date = file_div.find(:xpath, ".//div[label[@for='assignment_due_date']]")
      available_date = file_div.find(:xpath, ".//div[label[@for='assignment_available_date']]")
      yield options: inflate_box(bbox(due_date, available_date), 5, 5, 5, 5)

      team_submissions = file_div.find(:xpath, ".//div[label[text()='Team Submissions']]")
      yield options: inflate_box(bbox(team_submissions), 5, 5, 5, 5)

      time_taken_label = file_div.find(:xpath, ".//label[@for='assignment_request_time_taken']")
      time_taken_p = file_div.find(:xpath, ".//p[contains(., 'If selected, ask students on each submission how long')]")
      time_taken_toggle = file_div.find(:xpath, ".//div[input[@id='assignment_request_time_taken']]")
      yield options: inflate_box(bbox(time_taken_label, time_taken_p, time_taken_toggle), 5, 5, 5, 5)

      interlock_label = file_div.find(:xpath, ".//p[strong[text()='Interlocks']]")
      interlock_p = file_div.find(:xpath, ".//p[contains(., 'Restrict submissions to this assignment based on activity')]")
      interlock_button = file_div.find(:xpath, ".//a[@data-association='interlock']")
      hilite = highlight_area("interlock-button", bbox(interlock_button))
      yield options: inflate_box(bbox(interlock_label, interlock_p, hilite), 5, 5, 5, 5)
      remove_highlight "interlock-buton"

      default_late_penalty = file_div.find(:xpath, ".//div[./p[./strong[text()='Late Penalty']]]")
      yield options: inflate_box(bbox(default_late_penalty), 5, 5, 5, 5)

      graders = file_div.find(:xpath, ".//div[./ol[@id='files-graders']]")
      add_grader_button = graders.find("a")
      hilite = highlight_area("grader-button", bbox(add_grader_button))
      create_files = file_div.find(:xpath, ".//div[./input[@value='Create Files']]")
      yield options: inflate_box(bbox(graders, hilite, create_files), 5, 5, 5, 5)
      remove_highlight "grader-button"

      set_default_size
      # Remove the file so it doesn't throw an error when saving
      file_div.find(:xpath, ".//button[text()='Clear assignment file']").click
      add_grader_button.click
      create_files.find("input").click
      yield
    end

    def new_question_assignment_page
      sign_in(@fred)
      visit(course_assignments_path(@course))
      set_full_page

      page.find(:xpath, ".//a[text()='New']").click
      page.find(:xpath, ".//a[text()='Questions']").click
      questions_div = page.find(:xpath, ".//div[@id='type-questions']")
      name = questions_div.find(:xpath, ".//input[@id='assignment_name']")
      name.set("Course Contract")
      assignment_text = questions_div.find(:xpath, ".//textarea[@id='assignment_assignment']")
      assignment_text.set("Please read the <a href=\"https://course.ccs.neu.edu/cs2500/contract.html\">Course Contract</a> before submitting to this assignment.")
      title = page.find(:xpath, ".//strong[text()='Create New Assignment']")
      yield options: inflate_box(bbox(name, title, assignment_text), 5, 5, 5, 5)

      file_upload = questions_div.find(:xpath, ".//input[@id='assignment_assignment_file']", :visible => false)
      file_upload.set(File.expand_path('../../../test/fixtures/files/test-questions.yaml',  __FILE__))
      file_upload_div = questions_div.find(:xpath, ".//div[./p[@id='questions']]")
      yield options: inflate_box(bbox(file_upload_div), 5, 5, 5, 5)
      set_default_size
    end

    def new_code_review_assignment_page
      sign_in(@fred)
      visit(course_assignments_path(@course))
      set_full_page

      page.find(:xpath, ".//a[text()='New']").click
      page.find(:xpath, ".//a[text()='Code Review']").click
      code_review_div = page.find(:xpath, ".//div[@id='type-codereview']")
      name = code_review_div.find(:xpath, ".//input[@id='assignment_name']")
      name.set("Course Review of Assignment 1")
      assignment_text = code_review_div.find(:xpath, ".//textarea[@id='assignment_assignment']")
      assignment_text.set("Code review of <a href=\"https://course.ccs.neu.edu/cs2500/ps1.html\">assignment 1 </a>.")
      title = page.find(:xpath, ".//strong[text()='Create New Assignment']")
      yield options: inflate_box(bbox(name, title, assignment_text), 5, 5, 5, 5)

      code_review_div.find(:xpath, ".//select[@id='assignment_graders_attributes_0_review_target']").find("option[value='peer']").select_option
      who_to_review = code_review_div.find(:xpath, ".//div[./label[@for='assignment_graders_attributes_0_review_target']]")
      how_many_reviews = code_review_div.find(:xpath, ".//div[./label[@for='assignment_graders_attributes_0_review_count']]")
      yield options: inflate_box(bbox(who_to_review, how_many_reviews), 0, 5, 0, 0)

      review_quality_threshold = code_review_div.find(:xpath, ".//div[./label[@for='assignment_graders_attributes_0_review_threshold']]")
      yield options: inflate_box(bbox(review_quality_threshold), 0, 5, 0, 0)

      related_assignment = code_review_div.find(:xpath, ".//div[./label[@for='assignment_related_assignment_id']]")
      related_assignment_dropdown = code_review_div.find(:xpath, ".//select[@id='assignment_related_assignment_id']")
      hilite = highlight_area("related_assignment_dropdown", inflate_box(bbox(related_assignment_dropdown), 5, 5, 5, 5))
      #TODO: Fix dropdown disappearing when screenshotting (and remove hilite)
      #related_assignment_dropdown.click
      yield options: inflate_box(bbox(related_assignment, hilite), 5, 5, 5, 5)
      remove_highlight "related_assignment_dropdown"

      related_assignment_dropdown.find("option[value='1']").select_option
      related_assignment.find(:xpath, ".//label[contains(concat(' ',normalize-space(@class),' '),' active ')]").click
      # sleep so the toggle will complete
      sleep(0.5)
      yield options: inflate_box(bbox(related_assignment), 5, 5, 5, 5)

      set_default_size
    end

    def new_exam_assignment_page
      sign_in(@fred)
      visit(course_assignments_path(@course))
      set_full_page

      page.find(:xpath, ".//a[text()='New']").click
      page.find(:xpath, ".//a[text()='Exam']").click
      exam_div = page.find(:xpath, ".//div[@id='type-exam']")
      name = exam_div.find(:xpath, ".//input[@id='assignment_name']")
      name.set("Midterm exam")
      assignment_text = exam_div.find(:xpath, ".//textarea[@id='assignment_assignment']")
      assignment_text.set("To learn more about the exam please see the <a href=\"https://course.ccs.neu.edu/cs2500/\">course web page</a>.")
      title = page.find(:xpath, ".//strong[text()='Create New Assignment']")
      yield options: inflate_box(bbox(name, title, assignment_text), 5, 5, 5, 5)

      set_default_size
    end

    def style_graders
      sign_in(@fred)
      visit(course_assignments_path(@course))

      page.find(:xpath, ".//a[text()='New']").click
      set_full_page
      file_div = page.find(:xpath, ".//div[@id='type-files']")
      graders = file_div.find(:xpath, ".//div[./ol[@id='files-graders']]")
      graders.find("a").click
      points_available_label = file_div.find(:xpath, ".//td[.//label[text()='Points available:']]")
      points_available_value = file_div.find(:xpath, ".//td[.//input[contains(@id, 'avail_score')]]")
      yield options: inflate_box(bbox(points_available_label, points_available_value), 5, 5, 0, 5)

      file_div.find(:xpath, ".//a[text()='Racket Style']").click
      max_line_length_label = file_div.find(:xpath, ".//td[.//label[text()='Maximum line length:']]")
      max_line_length_value = file_div.find(:xpath, ".//td[.//input[contains(@id, 'line_length')]]")
      yield options: inflate_box(bbox(max_line_length_label, max_line_length_value), 5, 5, 0, 5)

      file_div.find(:xpath, ".//a[text()='Checker Tests']").click
      checker_test_table = file_div.find(:xpath, ".//table")
      yield options: bbox(checker_test_table)

      set_default_size
    end

    def edit_assignment
      sign_in(@fred)
      visit course_assignment_path(@course, @assignments[@assignments.length - 1])
      page.find(:xpath, ".//a[text()='Edit Assignment']").click
      graders = page.find(:xpath, ".//div[./ol[@id='files-graders']]")
      add_grader_button = graders.find(:xpath, ".//a[text()='Add Grader']")
      add_grader_button.click
      set_full_page
      update_files = page.find(:xpath, ".//div[./input[@value='Update Files']]")
      yield options: inflate_box(bbox(graders, add_grader_button, update_files), 5, 5, 5, 5)
      set_default_size

      page.find(:xpath, ".//input[@value='Update Files']").click
      yield
    end

    def registrations
      sign_in(@fred)
      visit(course_assignments_path(@course))
      registrations_button = page.find(:xpath, ".//a[text()='Registrations']")
      hilite = highlight_area("registrations_button", bbox(registrations_button))
      yield
      remove_highlight "registrations_button"

      registrations_button.click
      # TODO add registration request here
      # request = create(:reg_request, user: @students[0], course: @course, "#{@sections[0].type}_sections": @sections[0].crn.to_s)
      yield

      page.find(:xpath, ".//a[text()='Edit student registrations']").click
      title = page.find(:xpath, ".//h2[text()='Edit registrations']")
      student = page.all(:xpath, ".//tr[.//form[contains(@id, 'reg_lecture')]]")[0]
      section_dropdown = page.all(:xpath, ".//select[@id='role']")[0]
      section_dropdown.click
      section_dropdown.find("option[value='grader']").select_option
      section_dropdown.find("option[value='student']").select_option
      save_button = page.all(:xpath, ".//input[contains(@id, 'submit_lecture')]")[0]
      hilite = highlight_area("save_button", bbox(save_button))
      yield options: inflate_box(bbox(title, student), 5, 5, 5, 5)
      remove_highlight "save_button"

      visit(course_assignments_path(@course))
      page.find(:xpath, ".//a[text()='Registrations']").click
      impersonate = page.all(:xpath, ".//a[text()='Impersonate']")[0]
      hilite = highlight_area("impersonate", inflate_box(bbox(impersonate), 5, 5, 5, 5))
      row = page.all(:xpath, ".//tr[.//a[text()='Impersonate']]")[2]
      title = page.find(:xpath, ".//h3[./span[contains(text(), 'Students')]]")
      yield options: inflate_box(bbox(title, row, hilite), 0, 10, 0, 0)
      remove_highlight "impersonate"

      impersonate.click
      yield
      visit(course_path(@course))

      withdraw = page.find(:xpath, ".//button[contains(text(), 'Withdraw from this course')]")
      hilite = highlight_area("withdraw", bbox(withdraw))
      set_full_page
      yield
      remove_highlight "withdraw"
      set_default_size

      withdraw.click
      content = page.find(:xpath, ".//div[@class='modal-content']")
      set_full_page
      yield options: bbox(content)
      set_default_size
      page.find(:xpath, ".//a[contains(@href, 'withdraw')]").click
      set_full_page
      yield
      page.find(:xpath, ".//a[text()='Stop Impersonating']").click
      set_full_page
      yield
      set_default_size
    end
    
    def grading
      sign_in(@fred)
      visit course_assignment_path(@course, @assignments[3])
      assign_graders = page.find(:xpath, ".//a[@title='Assign graders']")
      highlight_area("assign_graders", bbox(assign_graders))
      yield
      remove_highlight "assign_graders"
      assign_graders.click
      
      grader_allocation = page.find(:xpath, ".//a[contains(@href, 'grader_allocations')]")
      highlight_area("grader_allocation", bbox(grader_allocation))
      yield
      remove_highlight "grader_allocation"
      
      grader_allocation.click
      page.first('#submission_id option').select_option
      page.select('Joe Jones', from: 'who_grades_id')
      assign_grader = page.find(:xpath, ".//input[@value='Assign grader']")
      highlight_area("assign_grader", bbox(assign_grader))
      header = page.find(:xpath, ".//h3[contains(text(), 'single grader')]")
      form = page.find(:xpath, ".//select[@id='who_grades_id']")
      set_full_page
      yield options: inflate_box(bbox(header, form), 10, 10, 10, 10)
      remove_highlight "assign_grader"
      set_default_size
      assign_grader.click
      
      header = page.find(:xpath, ".//h3[contains(text(), 'Existing')]")
      footer = page.find(:xpath, ".//footer")
      set_full_page
      yield options: bbox(header, footer)
      set_default_size
      
      joe_span = page.find(:xpath, ".//span[./span[text()='Joe Jones']]")
      joe_input = joe_span.find(:xpath, ".//input[contains(@id, '_weight')]")
      joe_input.set(2)
      assign_graders = page.find(:xpath, ".//input[@value='Assign graders']")
      highlight_area("assign_graders", bbox(assign_graders))
      header = page.find(:xpath, ".//h3[contains(text(), 'Bulk grading')]")
      or_header = page.find(:xpath, ".//h3[@class='middle-separator']")
      set_full_page
      yield options: inflate_box(bbox(header, or_header), 10, 10, 10, 10)
      remove_highlight "assign_graders"
      set_default_size
      assign_graders.click
      
      header = page.find(:xpath, ".//h3[contains(text(), 'Existing')]")
      footer = page.find(:xpath, ".//footer")
      set_full_page
      yield options: bbox(header, footer)
      set_default_size
      
      joe_header = page.find(:xpath, ".//h4[contains(text(), 'Joe Jones')]")
      abandon_all = joe_header.find(:xpath, ".//a[text() = 'Abandon all']")
      accept_alert do
        abandon_all.click
      end
      header = page.find(:xpath, ".//h3[contains(text(), 'Existing')]")
      footer = page.find(:xpath, ".//footer")
      set_full_page
      yield options: bbox(header, footer)
      set_default_size
    end
    
    def reviewing_grader_feedback
      sign_in(@fred)
      visit course_assignment_path(@course, @assignments[1])
      
      view_button = page.find_all(:xpath, ".//a[text() = 'View']")[0]
      highlight_area("view", bbox(view_button))
      yield
      remove_highlight "view"
      view_button.click
      
      manual_feedback_row = page.find(:xpath, ".//tr[./td[contains(text(), 'Manual Feedback')]]")
      grader_output = manual_feedback_row.find(:xpath, ".//a[text() = 'Grader output']")
      header = page.find(:xpath, ".//h1[text() = 'Submission']")
      highlight_area("grader_output", bbox(grader_output))
      set_full_page
      yield options: inflate_box(bbox(header, grader_output), 10, 10, 10, 10)
      set_default_size
      remove_highlight "grader_output"
    end
    
    def grader_statistics
      sign_in(@fred)
      visit(course_path(@course))

      grader_info = page.find(:xpath, ".//a[text() = 'Grader info']")
      highlight_area("grader_info", bbox(grader_info))
      yield
      remove_highlight "grader_info"
    end
    
    def downloading_teams
      sign_in(@fred)
      visit(course_path(@course))
      
      page.find(:xpath, ".//a[contains(text(), 'Export')]").click
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
Capybara.default_driver = :selenium_chrome_headless #:selenium_chrome
DatabaseCleaner.strategy = :deletion
DatabaseCleaner.start

path = Pathname.new(ARGV[1] || "./screenshots")
puts path
FileUtils.mkdir_p path unless File.directory? path
Screenshots.generate path
Screenshots.optimize path
