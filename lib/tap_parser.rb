require 'yaml'

class TapParser
  attr_reader :text, :test_count, :tests, :commentary, :passed_count, :time_taken
  attr_reader :filename

  def initialize(text, filename=nil)
    @filename = filename
    @text = text
    @test_count = 0
    @passed_count = 0
    @tests = []
    @commentary = []
    @line = 0
    if text != ""
      @lines = text.split("\n")
      parse_version
      parse_count
      parse_commentary
      while @lines.count > 0
        count = @lines.count
        parse_test
        if count == @lines.count # no progress
          print 
          raise "Could not parse TAP file (#{@filename}) at line #{@line}: #{@lines[0]}"
        end
      end
      @tests.length.upto(@test_count - 1) do |i|
        @tests[i] = missing_test(i + 1)
      end
    end
  end

  def next_line
    @line += 1
    @lines.shift
  end
  
  def missing_test(i)
    {num: i, passed: false, missing: true}
  end

  def parse_version
    mm = @lines[0].match(/^TAP version (\d+)$/)
    if mm
      @version = mm[1].to_i
      next_line
    end
  end

  def parse_count
    mm = @lines[0].match(/^1\.\.(\d+)$/)
    if mm
      @test_count = mm[1].to_i
      next_line
    end
  end

  def parse_commentary
    while @lines.length > 0 && @lines[0].match(/^# /)
      line = next_line[2..-1]
      @commentary.push(line)
      mm = line.match(/^Time: (.*)$/)
      if mm
        @time_taken = mm[1].to_f
      end
    end
  end

  def parse_test
    mm = @lines[0].match(/^(not\s+)?ok\b\s*(\d+)?\s*([^#]*)(#.*)?$/)
    if mm
      next_line
      passed = mm[1].nil?
      num = mm[2]
      comment = mm[3]
      directives = []
      if passed
        @passed_count += 1
      end
      if mm[4]
        directives.push(mm[4][2..-1])
      end
      if num.nil?
        num = @tests.length
      else
        num = num.to_i - 1
        @tests.length.upto(num - 1) do |i|
          @tests.push(missing_test(i + 1))
        end
      end
      while @lines.length > 0 && @lines[0].match(/^# /)
        directives.push(next_line[2..-1])
      end
      test = { num: num + 1, passed: passed, missing: false, info: {},
               comment: comment, directives: directives }
      if @lines.length > 0
        mm = @lines[0].match(/^(\s+)---\s*$/)
        if mm
          test[:info] = parse_info(mm[1]) || {}
        end
      end
      @tests.push test
    end
  end

  def make_yaml_safe(str)
    str.scrub do |bytes|
      "\u27e6#{bytes.unpack("H*")[0]}\u27e7" # square double brackets for invalid chars
    end.gsub(/[^\p{print}\p{space}]/) do |match|
      "\u27ea#{match.unpack("H*")[0]}\u27eb" # angled double brackets for unprintable chars
    end
  end

  def parse_info(indent)
    info = []
    regex = Regexp.new("^" + indent + "(.*)$")
    while @lines.length > 0 && (mm = @lines[0].match(regex))
      line = next_line
      break if line == (indent + "...")
      info.push(make_yaml_safe(mm[1]))
    end
    begin
      YAML.load(info.join("\n"))
    rescue Exception => ee
      # triple-backtick snarled things up
      print "Couldn't parse YAML (#{@filename}) for:\n``" + "`\n"
      print info.join("\n")
      print "\n`" + "``\n"
      print "Error was #{ee}\n"
    end
  end

  def points_available
    total_points = false

    # Find the total points
    @commentary.each do |comm|
      mm = comm.match(/TOTAL POINTS: (\d+(\.\d+)?)/)
      if mm
        total_points = mm[1].to_f
      end
    end

    if total_points == false
      total_points = 0
      @tests.each do |t|
        total_points += t[:info]["weight"] || 1
      end
    end

    total_points
  end

  def points_earned
    total_points = false
    # Find the total points
    @commentary.each do |comm|
      mm = comm.match(/TOTAL POINTS: (\d+(\.\d+)?)/)
      if mm
        total_points = mm[1].to_f
      end
    end

    if total_points == false
      total_points = 0
      @tests.each do |t|
        next if t[:passed] == false # skip the failed tests; they don't add anything
        next if t[:info]["suppressed"] # skip the suppressed tests
        total_points += t[:info]["weight"] || 1
      end
    else
      @tests.each do |t|
        next if t[:passed] # skip the passed tests; they don't deduct anything
        next if t[:info]["suppressed"] # skip the suppressed tests
        total_points -= t[:info]["weight"] || 1
      end
    end
    # Return the resulting points
    [0, total_points].max
  end

  def summary
    nums = @tests.map {|t| t[:num] }

    <<-"SUMMARY"
    Version:    #{@version}
    Test count: #{@test_count}
    Test data: #{@tests}
    Test data: #{nums}
    Commentary: #{@commentary}
    Num tests: #{@tests.length}
    Points earned: #{points_earned}
    SUMMARY
  end
end
