#!/usr/bin/env ruby

$stdout.sync = true
$stderr.sync = true

0.upto(20) do |ii|
  puts "Hello #{ii}"
  sleep(5)
end


