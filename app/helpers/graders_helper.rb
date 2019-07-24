module GradersHelper
  protected 
  def run_command_produce_tap(assignment, sub, timeout: Grader::DEFAULT_GRADING_TIMEOUT)
    g = self.grade_for sub
    u = sub.upload
    grader_dir = u.grader_path(g)

    grader_dir.mkpath

    prefix = "Assignment #{assignment.id}, submission #{sub.id}"

    tap_out, env, args, replacements, dir = get_command_arguments(assignment, sub)

    Audit.log("#{prefix}: Running #{self.type}.  Extracted dir: #{u.extracted_path}.  Running dir: #{dir || 'none given'}.  Timeout: #{timeout || 'unlimited'}.  Command line: #{args.join(' ')}")
    print("#{prefix}: Running #{self.type}.  Extracted dir: #{u.extracted_path}.  Running dir: #{dir || 'none given'}.  Timeout: #{timeout || 'unlimited'}.  Command line: #{args.join(' ')}\n")
    output, err, status, timed_out = ApplicationHelper.capture3(env, *args, chdir: dir&.to_s, timeout: timeout)
    File.open(grader_dir.join(tap_out), "w") do |tap|
      replacements&.each do |rep, with|
        output = output.gsub(rep, with)
      end
      tap.write(Upload.upload_path_for(output))
      g.grading_output = tap.path
    end
    if timed_out || !status&.success?
      Audit.log "#{prefix}: #{self.type} checker failed: timed out #{timed_out}, status #{status}, error: #{err}"
      yield prefix, {timed_out: timed_out, status: status, output: output, err: err}, g, nil
      return 0
    else
      begin
        tap = TapParser.new(output)
        Audit.log "#{prefix}: #{self.type} checker results: Tap: #{tap.points_earned}"
        yield prefix, nil, g, tap
        return self.avail_score.to_f * (tap.points_earned.to_f / tap.points_available.to_f)
      rescue Exception => e
        yield prefix, {tap: e}, g, nil
        return 0
      end
    end
  end
  def run_build_produce_problems(assignment, sub, timeout: Grader::DEFAULT_COMPILE_TIMEOUT)
    g = self.grade_for sub
    u = sub.upload
    grader_dir = u.grader_path(g)

    grader_dir.mkpath

    prefix = "Assignment #{assignment.id}, submission #{sub.id}"

    dest, grader_path, sub_path, assets = get_extraction_arguments(assignment, sub)
    prepare_build_dir(prefix, dest, grader_path, sub_path, assets)
    details_out, env, cmds, replacements, build_dir = get_build_arguments(assignment, sub)
    any_problems = false
    File.open(grader_dir.join(details_out), "w") do |details|
      details.write "Contents of temp directory are:\n"
      ls_output, status = Open3.capture2("ls", "-R", build_dir.to_s)
      details.write ls_output
      cmds.each do |cmd|
        details.write("#{prefix}: Compiling `#{cmd.join(' ')}\n")
        Audit.log "#{prefix}: Compiling `#{cmd.join(' ')}`"
        comp_out, comp_err, comp_status, timed_out =
                                         ApplicationHelper.capture3(*cmd,
                                                                    chdir: build_dir.to_s,
                                                                    timeout: timeout)
        details.write("#{prefix}: (exit status #{comp_status})\n")
        details.write(comp_out)
        if timed_out
          details.write("#{prefix}: Compilation timed out after #{timeout} seconds")
        end
        if timed_out || !comp_status&.success?
          details.write("Errors building #{cmd.join(' ')}:\n")
          details.write(comp_err)
          Audit.log("#{prefix}: failed with compilation errors; see #{details_out}")
          any_problems = true
        end
      end
      details.write "Contents of temp directory are now:\n"
      ls_output, status = Open3.capture2("ls", "-R", build_dir.to_s)
      details.write ls_output
      yield prefix, any_problems, details
    end
  end

  def get_extraction_arguments(assignment, sub)
    fail NotImplementedError, "Must implement this if you're using run_command_produce_problems"
  end
  def get_build_arguments(assignment, sub)
    fail NotImplementedError, "Must implement this if you're using run_command_produce_problems"
  end
  def get_command_arguments(assignment, sub)
    fail NotImplementedError, "Must implement this if you're using run_command_produce_tap"
  end

  def record_compile_error(sub, g)
    InlineComment.transaction do
      InlineComment.where(submission: sub, grade: g).destroy_all

      g.score = 0
      g.out_of = self.avail_score
      g.updated_at = DateTime.now
      g.available = true
      g.save!

      InlineComment.create!(
        submission: sub,
        title: "Compilation errors",
        filename: Upload.upload_path_for(sub.upload.extracted_path.to_s),
        line: 0,
        grade: g,
        user: nil,
        label: "general",
        severity: InlineComment::severities["error"],
        weight: self.avail_score,
        comment: "Could not parse your program, so could not compute any style points at all",
        suppressed: false)
    end
  end
  def record_tap_as_comments(g, tap, sub)
    InlineComment.transaction do
      g.score = tap.points_earned
      g.out_of = tap.points_available
      g.updated_at = DateTime.now
      g.available = true
      g.save!

      InlineComment.where(submission: sub, grade: g).destroy_all
      ics = tap.tests.map do |t|
        puts "Severity is #{t[:info]}"
        InlineComment.new(
          submission: sub,
          title: t[:comment],
          filename: Upload.upload_path_for(t[:info]["filename"]),
          line: t[:info]["line"],
          grade: g,
          user: nil,
          label: t[:info]["category"],
          severity: InlineComment::severities[t[:info]["severity"].humanize(:capitalize => false)],
          comment: t[:info]["message"],
          weight: t[:info]["weight"],
          suppressed: t[:info]["suppressed"])
      end
      InlineComment.import ics
    end
  end


  def copy_srctest_from_to(from, to, prefix = "")
    if !prefix.empty?
      prefix = "#{prefix}: "
    end
    if to.kind_of? String
      to = Pathname.new(to)
    end
    if from.kind_of? String
      from = Pathname.new(from)
    end
    # preserves when the submission contains src/ and test/ directories
    # or creates them if only sources are submitted
    # to.join("src").mkpath
    # to.join("test").mkpath
    
    if Dir.exists?(from.join("src")) && Dir.exists?(from.join("test"))
      Audit.log("#{prefix}From = #{from} and contains src/ and test/")
      # FileUtils.cp_r("#{from.join('src')}/.", "#{to.join('src')}/")
      # FileUtils.cp_r("#{from.join('test')}/.", "#{to.join('test')}/")
      FileUtils.cp_r("#{from.join('src')}/.", "#{to}", remove_destination: true)
      FileUtils.cp_r("#{from.join('test')}/.", "#{to}", remove_destination: true)
    else
      Audit.log("#{prefix}From = #{from} and does not contain src/ and test/")
      # FileUtils.cp_r("#{from}/.", "#{to.join('src')}/")
      FileUtils.cp_r("#{from}/.", "#{to}/", remove_destination: true)
    end
  end

  def prepare_build_dir(prefix, dest, grader_path, sub_path, assets)
    Audit.log("#{prefix}: Preparing destination #{dest}")
    if (Dir.exists?(grader_path.join("starter")) &&
        Dir.exists?(grader_path.join("testing")))
      copy_srctest_from_to(grader_path.join("starter"), dest, prefix)
    end
    copy_srctest_from_to(sub_path, dest, prefix)
    assets.each do |asset|
      FileUtils.cp(asset, dest)
    end
    if (Dir.exists?(grader_path.join("starter")) &&
        Dir.exists?(grader_path.join("testing")))
      copy_srctest_from_to(grader_path.join("testing"), dest, prefix)
    else
      copy_srctest_from_to(grader_path, dest, prefix)
    end
  end

end
