require 'active_record'

APP_PATH = File.expand_path('../../config/application',  __FILE__)
require File.expand_path('../../config/boot',  __FILE__)
require APP_PATH
Rails.application.require_environment!

InlineComment
  .joins(:submission).where("submissions.assignment_id": ARGV[0].to_i)
  .joins(:grade).where("grades.grader_id": ARGV[1].to_i)
  .group_by(&:submission)
  .each do |sub, cs|
  if ARGV[2]
    match = ARGV[2].downcase
    cs = cs.keep_if{|c| c.comment.downcase.include?(match)}
  end
  next if cs.empty?
  print "#{sub.team&.to_s || sub.user.name} -- Sub ##{sub.id}:\n"
  cs.each do |c|
    print "#{c.filename.gsub(Regexp.new('.*extracted/?'), '')}, #{c.line}: #{c.label}, #{c.severity} [#{c.weight}]: #{c.comment}\n"
  end
  print "==================================================\n\n"
end
