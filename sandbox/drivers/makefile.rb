#!/usr/bin/env ruby

sub = ENV["BN_SUB"]
gra = ENV["BN_GRA"]
key = ENV["BN_KEY"]
ENV["BN_KEY"] = ""

def sub_dir
  Dir.chdir "/home/student/submission"
  count = %x{find `pwd` -name "Makefile" | wc -l}.chomp.to_i
  if count > 1
    raise Exception.new("Too many Makefiles: #{count}")
  elsif count == 0
    raise Exception.new("No Makefiles found")
  end

  %x{dirname $(find `pwd` -name "Makefile" | head -1)}.chomp
end

def run_in_sub(cmd)
  dir = sub_dir
  system(%Q{chown -R student:student "#{dir}"})
  Dir.chdir dir
  system(%Q{su - student -c "cd #{dir}; #{cmd}"})
end

def unpack_or_copy(file, where)
  case
  when (file.match(/\.tar\.gz$/i) or file.match(/\.tgz$/i))
    puts(%Q{(cd #{where} && tar xzvf "#{file}")})
    system(%Q{(cd #{where} && tar xzvf "#{file}")})
  when file.match(/\.zip$/i)
    puts(%Q{(cd #{where} && unzip -o "#{file}")})
    system(%Q{(cd #{where} && unzip -o "#{file}")})
  else
    puts(%Q{(cd #{where} && cp -f "#{file}" .)})
    system(%Q{(cd #{where} && cp -f "#{file}" .)})
  end
end

puts "== Unpacking grading tarball first..."
unpack_or_copy(gra, "/home/student")
system(%Q{(cd /home/student && cp -r starter/* submission/)})

puts "== Unpacking student code..."
unpack_or_copy(sub, "/home/student/submission")

puts "== Building code..."
run_in_sub("make")

puts "== Unpacking grading tarball over submission..."
system(%Q{(cd /home/student && cp -r testing/* submission/)})

puts "== Building tests..."
run_in_sub("make test")
puts "== Running tests..."
puts key
run_in_sub("make run-test")
puts key
puts "Makefile driver, grading done."

