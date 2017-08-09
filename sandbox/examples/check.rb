#!/usr/bin/env ruby

def usage
  puts "Usage: ruby check.rb sub.tar.gz grading.tar.gz"
  exit(1)
end

def shell(cmd)
  cmd.gsub!(%q{"}, %q{\"})
  system(%Q{bash -c "#{cmd}"}) or
    raise Exception.new("Command '#{cmd}' failed: #{$?}")
end

sub = ARGV.shift or usage
gra = ARGV.shift or usage
dir = "tmp.#{$$}"

shell(%Q{mkdir -p "#{dir}"})
shell(%Q{cp "#{sub}" "#{dir}"})
shell(%Q{(cd "#{dir}" && tar xzvf "../#{gra}")})
shell(%Q{(cd "#{dir}" && SUB="#{sub}" ruby -I_grading _grading/grade.rb)})
shell(%Q{rm -r "#{dir}"}) if dir =~ /^tmp/

