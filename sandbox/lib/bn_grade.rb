require 'json'

def run(cmd)
  puts "Running: #{cmd}"
  system(cmd) or begin
      puts "Command failed: #{cmd}"
      exit(1)
    end
end

def unpack_to_home(file)
  Dir.chdir "/home/student"

  if (file =~ /\.tar\.gz$/i) || file =~ (/\.tgz$/i)
    run(%Q{tar xzf "#{file}"})
  elsif (file =~ /\.zip/i)
    run(%Q{unzip "#{file}"})
  else
    run(%Q{cp "#{file}" .})
  end
end

def unpack_submission
  if ENV["BN_SUB"]
    unpack_to_home(ENV["BN_SUB"])
  end
end

def unpack_grading
  if ENV["BN_GRADE"]
    unpack_to_home(ENV["BN_GRADE"])
  end
end

class BnScore
  def initialize
    @tst_no = 1
    @bn_key = ENV["BN_KEY"] || "-- bn security key --"
    ENV["BN_KEY"] = ""

    @scores = []
  end

  def add(txt, pts, max)
    @scores << [txt, pts, max]
  end

  def test(txt, max)
    pts = yield
    pts = 1 if pts == true
    pts = 0 if pts == false

    add(txt, pts, max)

    puts "Test\t##{@tst_no}:\t#{pts} / #{max}\t#{txt}"
  end
  
  def output!
    pts = 0
    max = 0

    @scores.each do |_, pp, mm|
      pts += pp
      max += mm
    end

    puts
    puts @bn_key
    puts({
      scores: @scores,
      pts: pts,
      max: max,
    }.to_json)
  end
end

