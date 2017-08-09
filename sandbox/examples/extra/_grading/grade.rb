#!/usr/bin/env ruby

require 'steno'

fs = ['Makefile', 'test.pl']

FileUtils.cp("test.pl", "_grading")

steno = Steno.new
steno.save_grading_hashes(fs)
steno.unpack
steno.shell("make")
steno.check_grading_hashes
fs.each do |name|
  FileUtils.cp("_grading/#{name}", ".")
end
steno.run_tests("perl test.pl")

