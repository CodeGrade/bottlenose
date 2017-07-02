require 'securerandom'
require 'tap_parser'
require 'audit'
require 'container'

class SandboxGrader < Grader
  def autograde?
    true
  end

  def display_type
    "Sandboxed Script"
  end

  def to_s
    if self.upload
      "#{self.avail_score} points: Run sandboxed script, using #{self.upload.file_name}"
    else
      "#{self.avail_score} points: Run sandboxed script"
    end
  end

  protected

  def do_grading(assignment, sub)
    g = self.grade_for sub

    sandbox = Sandbox.create(name: "Manual grader", submission: sub)
    cont    = sandbox.container

    self.save
    grade = 0.0

    begin
      grade = run_sandbox(assignment, sub, sandbox, cont)
    rescue Exception => e
      Audit.log "Assignment #{assignment.id}, submission #{sub.id}: Sandbox grader failed"
      Audit.log e.inspect
    end

    sandbox.stop_container
    unless sandbox.destroy
      Audit.log "Assignment #{assignment.id}, submission #{sub.id}: could not destroy sandbox"
      Audit.log sandbox.errors.inspect
    end
    return grade
  end

  def run_sandbox(assignment, sub, sandbox, cont)
    g = self.grade_for sub
    g.score = 0
    g.out_of = 50
    g.available = false
    g.save!

    file = sub.upload.submission_path
    secret = SecureRandom.hex(16)

    self.save
    grade = 0.0

    prefix = "Assignment #{assignment.id}, submission #{sub.id}"

    begin
      grade = run_sandbox(assignment, sub, sandbox, cont)
      Audit.log "#{prefix}: Sandbox grader complete\n"
    rescue Exception => e
      Audit.log "#{prefix}: Sandbox grader failed\n"
      Audit.log "#{e.inspect}\n"
    end

    sandbox.stop_container
    sandbox.destroy or Audit.log "#{prefix}: #{sandbox.errors.inspect}\n"
    return grade
  end

  def run_sandbox(assignment, sub, sandbox, cont)
    g = self.grade_for sub
    g.score = 0
    g.out_of = 50
    g.available = false
    g.save!

    u = sub.upload

    grader_dir = u.grader_path(g)
    grader_dir.mkpath

    prefix = "Assignment #{assignment.id}, submission #{sub.id}"

    file = u.submission_path
    secret = SecureRandom.hex(16)

    cont.start!
    cont.mkdir("/tmp/bn")
    cont.chmod("/tmp/bn", 0711)
    cont.mkdir("/tmp/bn/#{secret}")
    cont.push_dir(Rails.root.join('sandbox', 'lib'), "/tmp/bn")

    cont.mkdir("/home/student/submission")
    sandbox_sub_path = "/home/student/#{u.file_name}"
    cont.push(file, sandbox_sub_path)
    sandbox_sub_path = "/tmp/bn/#{secret}/#{sub.upload.file_name}"
    cont.push(file, sandbox_sub_path)

    sandbox_gra_path = "/tmp/bn/#{self.upload.file_name}"
    cont.push(self.upload.submission_path, sandbox_gra_path)
    cont.push_dir(self.upload.extracted_path, "/tmp/bn/#{secret}")

    if File.exists?(self.upload.extracted_path.join('extracted', 'script'))
      cont.chmod("/tmp/bn/#{secret}/extracted/script", 0755)
    end

    driver = Rails.root.join('sandbox', 'drivers', params)
    unless File.exists?(driver)
      Audit.log "#{prefix}: FAILURE: Sandbox grader wasn't configured with a driver\n"
      return
    end
    Audit.log("#{prefix}: Starting sandbox grader with secret #{secret}\n");
    stdout, stderr, rv = cont.exec_driver(driver, secret, sandbox_sub_path, sandbox_gra_path)

    parts = stdout.split("#{secret}\n")
    g.score = 0
    g.out_of = 50

    details_log = grader_dir.join("details.log")
    makefile_tap = grader_dir.join("makefile.tap")
    File.open(details_log, "w") do |details|
      details.write "== stdout ==\n\n#{stdout}\n\n== stderr ==\n\n#{stderr}\n\n== end of output ==\n"
    end
    if parts.size >= 3
      begin
        tap = TapParser.new(parts[1])
        Audit.log "#{prefix}: Sandbox grader results: Tap: #{tap.points_earned}\n"

        File.open(makefile_tap, "w") do |makefile|
          makefile.write(parts[1])
        end

        g.score = tap.points_earned
        g.out_of = tap.points_available
        g.grading_output = makefile_tap.to_s
      rescue Exception => e
        Audit.log "#{prefix}: Could not parse Tap results; see #{details_log}\n"
        g.score = 0
        g.out_of = 1
        g.grading_output = details_log.to_s
      end
    else
      Audit.log "#{prefix}: Sandbox grader failed: did not find at least three parts of output (expected secret #{secret}); see #{details_log}\n"
      g.score = 0
      g.out_of = 1
      g.grading_output = details_log.to_s
    end

    g.updated_at = DateTime.now
    g.available = true
    g.save!

    if parts.size < 3
      Audit.log "Assignment #{assignment.id}, submission #{sub.id}: Can't parse sandbox output: Not enough parts"
      Audit.log "Expected secret: #{secret}"
      Audit.log "== Output =="
      Audit.log stdout
      Audit.log "== Parts =="
      Audit.log parts.inspect
      Audit.log "==========="
    else
      tap = TapParser.new(parts[1])
      Audit.log "Assignment #{assignment.id}, submission #{sub.id} sandbox grader results: Tap: #{tap.points_earned}\n"

      g.score = tap.points_earned
      if tap.points_available >= 1.0
        g.out_of = tap.points_available
      end
      g.updated_at = DateTime.now

      score = g.score / g.out_of
      unless (score < 5 && score > -0.1)
        g.score = 0
        g.out_of = 50
      end
    end

    g.save!

    # puts g.notes
    sandbox.destroy!

    return self.avail_score.to_f * (g.score.to_f / g.out_of.to_f)
  end
end
