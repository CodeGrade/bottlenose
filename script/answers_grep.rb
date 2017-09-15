require 'active_record'
require 'yaml'

assn_num = ARGV.shift.to_i
grep_cmds = YAML.load(ARGF.read)

APP_PATH = File.expand_path('../../config/application',  __FILE__)
require File.expand_path('../../config/boot',  __FILE__)
require APP_PATH
Rails.application.require_environment!

def match(ans, query, index)
  return nil if query.nil?
  query["op"] ||= "=="
  case query["op"]
  when "noop"
  when "or"
    found = false
    query["opts"].each do |q|
      a = match(ans, q)
      found = true if a
    end
    return ans if found
  when "ans"
    found = true
    query["opts"].each do |q|
      a = match(ans, q)
      found = false unless a
    end
    return ans if found
  when "=="
    return ans if ans["main"] == query["val"].to_s
  when "!="
    return ans if ans["main"] != query["val"].to_s
  end
  return nil
end

Assignment.find(assn_num.to_i).submissions.each do |sub|
  answers = YAML.load(File.open(sub.upload.submission_path))
  matches = []
  answers.zip(grep_cmds).each_with_index do |(ans, query), index|
    matches.push (match(ans, query, index))
  end
  matches.compact!
  if !matches.empty?
    print "#{sub.id}: #{sub.user.name} (NUID #{sub.user.nuid})"
    if sub.team
      print " (#{sub.team.to_s})"
    end
    print ":\n"
    print matches
    print "\n"
  end
end
