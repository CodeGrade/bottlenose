Rails.env = ENV["RAILS_ENV"] = "test"
require File.expand_path('../../../config/environment', __FILE__)
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
  def set_window_size(width = nil, height = nil)
    window = Capybara.current_session.current_window
    if width.nil? || height.nil?
      w, h = window.size()
      width = width || w
      height = height || h
    end
    window.resize_to(width, height)
  end
  def sign_in(who)
    Warden.test_mode!
    Warden.test_reset!
    login_as(who, scope: warden_scope(who))
  end
  def warden_scope(*resource)
    resource.class.name.underscore.to_sym
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
end

class Screenshots
  include Capybara::DSL
  include FactoryBot::Syntax::Methods
  include Utilities

  BROWSER_WIDTH  = 1280
  BROWSER_HEIGHT = 768

  def self.setup(*methods)
    methods.each do |m|
      send(self, m)
    end
  end

  def self.generate(path, width: BROWSER_WIDTH, height: BROWSER_HEIGHT)
    Capybara.save_path = path
    Headless.ly do
      DatabaseCleaner.clean_with :truncation
      
      ss = Screenshots.new
      ss.set_window_size(width, height)
      Screenshots.instance_methods(false).each do |method|
        ss.send(method) do |file = method, options: {}|
          filename = "#{file}.png"
          ss.page.save_screenshot(filename)
          if options["left"] && options["top"]
            if options["right"] && options["bot"]
              options["width"] = options["right"] - options["left"]
              options["height"] = options["bot"] - options["top"]
            end
            if options["width"] && options["height"]
              geometry = "#{options["width"]}x#{options["height"]}+#{options["left"]}+#{options["top"]}"
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
        DatabaseCleaner.clean
      end
      
      dir = Upload.base_upload_dir.to_s
      if Rails.env === "manual" && dir =~ /manual/
        FileUtils.rm_rf(dir)
      end
    end
  end

  def self.optimize(path)
    Headless.ly do
      Dir["#{path}/**"].each do |filename|
        puts "Optimizing #{filename}"
        `pngquant --force --output #{filename} #{filename}`
      end
    end
  end

  #######################################################
  
  def homepage
    visit("/")
    yield
    yield "excerpt", options: bbox(page.find_all("input.form-control"), page.find_all("label"))
  end

  def first_run
    @course = create(:course, footer: nil)
    @fred = create(:user, name: "Fred McTeacher", first_name: "Fred", last_name: "McTeacher")
    @section = create(:section, course: @course, instructor: @fred, crn: 31415)
    @assignments = (1..5).map do |i|
      ts = create(:teamset, course: @course, name: "Teamset #{i}")
      create(:assignment, name: "Assignment #{i}", course: @course, teamset: ts)
    end
    @assignments.map(&:save!)
    @fred_reg = create(:registration, course: @course, user: @fred,
                       role: Registration::roles[:professor], show_in_lists: false, new_sections: [@section.crn])
    @fred_reg.save_sections
    login_as(@fred, scope: :user)
    visit("/")
    yield "initial-profile"
    visit(course_path(@course))
    yield "course-home"
    page.click_link("Assignments")
    yield "assignments-home"
  end

end

desc "Generates screenshots for the manual"
namespace :manual do
  task :screenshots => :environment do
    FactoryBot.find_definitions
    include Rails.application.routes.url_helpers
    Capybara.default_driver = :selenium_chrome
    DatabaseCleaner.strategy = :deletion
    DatabaseCleaner.start
    
    path = Pathname.new(ARGV[1] || "./screenshots")
    puts path
    FileUtils.mkdir_p path unless File.directory? path
    Screenshots.generate path
    Screenshots.optimize path
  end
end
