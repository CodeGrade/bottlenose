#!/usr/bin/env ruby
#require 'bn_grade'

key = ENV['BN_KEY']
sub = ENV['BN_SUB']
gra = ENV['BN_GRA']
ENV['key'] = ""

puts "Grading with driver: complex.rb"

raise Exception.new("TODO: proper grading driver")

# TODO:
#  - Run student build system, capture stdout and stderr
#  - Run student program, capture stdout and stderr
#  - Report results of grading script, not spoofable by student.
#  - All data must be streamed so it can be displayed to the student live.

puts key

puts key
puts "Grading driver done: complex.rb

