#!/usr/bin/env ruby

require 'steno'

fs = ['Makefile', 'test.pl']

steno = Steno.new
steno.save_grading_hashes(fs)
steno.unpack
steno.shell("make")
steno.check_grading_hashes
fs.each do |name|
  FileUtils.cp("_grading/#{name}", ".")
end
steno.run_tests("make test")

