require 'active_record'

course_num = ARGV[0].to_i

APP_PATH = File.expand_path('../../config/application',  __FILE__)
require File.expand_path('../../config/boot',  __FILE__)
require APP_PATH
Rails.application.require_environment!

data = [["Assignment", "Min", "Average", "Median", "Stddev", "Max", "Max (with outliers)"]]

module Enumerable
  def mean
    self.sum.to_f / self.length.to_f
  end

  def variance
    m = self.mean
    sum = self.reduce(0) {|acc, i| acc + (i - m)**2}
    sum / (self.length - 1).to_f
  end

  def stddev
    Math.sqrt(self.variance)
  end

  def median
    self.sort[self.length / 2]
  end
end
    
course = Course.find(course_num.to_i)
course.assignments.order(:due_date, :available, :name).each do |assn|
  if assn.request_time_taken?
    max_possible = (assn.due_date - assn.available) / 1.hours.to_f
    times = assn.used_submissions.map(&:time_taken).compact
    times, jokes = times.partition{|t| t < max_possible && t >= 0}
    joke_max = if jokes.blank? then "N/A" else jokes.max end
    data.push([assn.name, times.min, times.mean, times.median, times.stddev, times.max, joke_max])
  else
    data.push([assn.name, "--", "--", "--", "--", "--", "--"])
  end
end

data.each do |row|
  (1..row.size).each do |col|
    if (row[col].is_a? Integer) || (row[col].is_a? Float)
      row[col] = row[col].round(3)
    end
  end
end

padding = (0..data[0].size).map{|_| 0}
data.each do |row|
  (0..row.size).each do |col|
    padding[col] = [padding[col], row[col].to_s.length].max
  end
end
data.each do |row|
  (0..row.size).each do |col|
    row[col] = row[col].to_s + (" " * (padding[col] - row[col].to_s.length))
  end
end
name = course.name + " (" + course.term.name + ")"
puts name
puts ("=" * name.length)
data.each do |row|
  puts row.join("  ")
end
