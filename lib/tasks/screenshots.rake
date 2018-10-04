desc "Generates screenshots for the manual"
namespace :manual do
  task :screenshots => :environment do
    exec("ruby", File.expand_path("../screenshots.rb", __FILE__), *ARGV[1..-1])
  end
end
