#!/usr/bin/env ruby
#require 'bn_grade'

key = ENV['BN_KEY']
sub = ENV['BN_SUB']
gra = ENV['BN_GRA']
ENV['key'] = ""

puts "Grading with driver: lazy.rb"
puts key

system(%Q{chmod 0755 "#{gra}"})
system(%Q{chown -R student /home/student})
system(%Q{(cd ~student && su student -c 'RUBYLIB="/tmp/bn/lib" "#{gra}"')})

puts key
puts "Grading driver done: lazy.rb"

