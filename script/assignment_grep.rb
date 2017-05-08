require 'active_record'

assn_num = ARGV[0].to_i
grep_cmd = ARGV[1]

APP_PATH = File.expand_path('../../config/application',  __FILE__)
require File.expand_path('../../config/boot',  __FILE__)
require APP_PATH
Rails.application.require_environment!

Assignment.find(assn_num.to_i).submissions.each do |sub|
  to_run = grep_cmd.gsub("{0}", sub.upload.extracted_path.to_s)
  ans = `#{to_run}`
  if ans != ""
    print "#{sub.id}: #{sub.user.name}"
    if sub.team
      print " (#{sub.team.to_s})"
    end
    print ":\n"
    print ans
    print "\n"
  end
end
