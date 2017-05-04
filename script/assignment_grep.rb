require 'active_record'

assn_num = ARGV[0].to_i
grep_cmd = ARGV[1]

APP_PATH = File.expand_path('../../config/application',  __FILE__)
require File.expand_path('../../config/boot',  __FILE__)
require APP_PATH
Rails.application.require_environment!

Assignment.find(assn_num.to_i).submissions.each do |sub|
  needs_cleanup = Dir.exist?(sub.upload.extracted_path)
  sub.upload.extract_contents!
  to_run = grep_cmd.gsub("{0}", sub.upload.extracted_path.to_s)
  ans = `#{to_run}`
  if needs_cleanup
    sub.upload.cleanup_extracted!
  end
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
