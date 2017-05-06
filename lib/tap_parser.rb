require 'yaml'

class TapParser
  attr_reader :text, :test_count, :tests, :commentary, :passed_count

  def initialize(text)
    @text = text
    @test_count = 0
    @passed_count = 0
    @tests = []
    @commentary = []
    if text != ""
      lines = text.split("\n")
      parse_version(lines)
      parse_count(lines)
      parse_commentary(lines)
      while lines.count > 0
        count = lines.count
        parse_test(lines)
        if count == lines.count # no progress
          raise "Could not parse TAP file at line #{lines[0]}"
        end
      end
      @tests.length.upto(@test_count - 1) do |i|
        @tests[i] = missing_test(i + 1)
      end
    end
  end

  def missing_test(i)
    {num: i, passed: false, missing: true}
  end

  def parse_version(lines)
    mm = lines[0].match(/^TAP version (\d+)$/)
    if mm
      @version = mm[1].to_i
      lines.shift
    end
  end

  def parse_count(lines)
    mm = lines[0].match(/^1\.\.(\d+)$/)
    if mm
      @test_count = mm[1].to_i
      lines.shift
    end
  end

  def parse_commentary(lines)
    while lines.length > 0 && lines[0].match(/^# /)
      @commentary.push(lines.shift[2..-1])
    end
  end

  def parse_test(lines)
    mm = lines[0].match(/^(not )?ok\b\s*(\d+)?([^#]*)(#.*)?$/)
    if mm
      lines.shift
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
      while lines.length > 0 && lines[0].match(/^# /)
        directives.push(lines.shift[2..-1])
      end
      test = { num: num + 1, passed: passed, missing: false, info: {},
               comment: comment, directives: directives }
      if lines.length > 0
        mm = lines[0].match(/^(\s+)---\s*$/)
        if mm
          test[:info] = parse_info(lines, mm[1]) || {}
        end
      end
      @tests.push test
    end
  end

  def parse_info(lines, indent)
    info = []
    regex = Regexp.new("^" + indent + "(.*)$")
    while lines.length > 0 && (mm = lines[0].match(regex))
      line = lines.shift
      break if line == (indent + "...")
      # nuke any ANSI escape codes
      info.push(mm[1].gsub("\u001B", "ESC").gsub("\b", "\\b"))
    end
    begin
      YAML.load(info.join("\n"))
    rescue Exception => ee
      # triple-backtick snarled things up
      print "Couldn't parse YAML for:\n``" + "`\n"
      print info.join("\n")
      print "\n`" + "``\n"
      print "Error was #{ee}\n"
    end
  end

  def points_available
    total_points = false

    # Find the total points
    @commentary.each do |comm|
      mm = comm.match(/TOTAL POINTS: (\d+)/)
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
      mm = comm.match(/TOTAL POINTS: (\d+)/)
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
