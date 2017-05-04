require 'active_record'

InlineComment
  .joins(:submission).where("submissions.assignment_id": ARGV[0].to_i)
  .joins(:grader).where("graders.grader_config_id": ARGV[1].to_i)
  .group_by(&:submission)
  .each do |sub, cs|
  cs.each do |c|
    print "#{c.user.name} -- #{c.submission_id}, #{c.filename.gsub(Regexp.new('.*extracted/?'), '')}, #{c.line}: #{c.label}, #{c.severity} [#{c.weight}]: #{c.comment}\n"
  end
  print "==================================================\n\n"
end
