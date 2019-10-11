require 'sub_tarball'

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
      start_time = Time.now
      details.puts("#{prefix}: Build began at #{start_time}")
      details.puts("#{prefix}: Contents of temp directory are:")
      tree_output = dir_tree(build_dir)
      details.puts tree_output
      cmds.each do |cmd|
        if (cmd.is_a? Hash)
          if cmd[:skip].call
            cmd = cmd[:cmd]
            details.puts("#{prefix}: Skipping `#{cmd.join(' ')}`, because it's already compiled")
            next
          else
            cmd = cmd[:cmd]
          end
        end
        details.puts("#{prefix}: Running `#{cmd.join(' ')}`")
        Audit.log "#{prefix}: Running `#{cmd.join(' ')}`"
        comp_out, comp_err, comp_status, timed_out =
                                         ApplicationHelper.capture3(*cmd,
                                                                    chdir: build_dir.to_s,
                                                                    timeout: timeout)
        details.puts("#{prefix}: (exit status #{comp_status})")
        details.puts(comp_out)
        if timed_out
          details.puts("#{prefix}: Compilation timed out after #{timeout} seconds")
        end
        if timed_out || !comp_status&.success?
          details.puts("#{prefix}: Errors building #{cmd.join(' ')}:")
          details.puts(comp_err)
          Audit.log("#{prefix}: failed with compilation errors; see #{details_out}")
          any_problems = true
        end
      end
      details.puts("Contents of temp directory are now:")
      tree_output = dir_tree(build_dir)
      details.puts(tree_output)
      end_time = Time.now
      details.puts("Build ended at #{end_time}")
      details.puts("Total build time: #{end_time - start_time} seconds")
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

  def export_tap_data
    tb = SubTarball.new(self.assignment)
    tb.update_with!(
      self.assignment.used_submissions.includes(:users, :grades, upload: [:course, :assignment])
        .where("grades.grader_id": self.id).map do |s|
        upload_path = Upload.upload_path_for(s.upload.extracted_path)
        g = s.grades.first.grading_output_path
        if File.exists? g
          ["#{s.id}#{File.extname(g)}", File.read(g).gsub("#{upload_path}/", "")]
        else
          ["#{s.id}.missing", ""]
        end
      end.to_h
    )
    tb
  end

  def import_tap_data(who_grades, file)
    ans = {created: 0, updated: 0, errors: []}
    Dir.mktmpdir("regrade-#{self.id}_") do |tmpdir|
      ArchiveUtils.extract(file.path, file.content_type, tmpdir, force_readable: true)
      if Dir.exists?("#{tmpdir}/assignment_#{self.assignment_id}")
        entries = Dir.glob("#{tmpdir}/assignment_#{self.assignment_id}/*")
      elsif Dir.glob("#{tmpdir}/assignment_*").length > 0
        ans[:errors] << "Found a directory that appears to be for a different assignment; is this the wrong upload?"
        entries = []
      else
        entries = Dir.glob("#{tmpdir}/*")
      end
      entries = entries.select{|f| File.file? f}.group_by{|f| File.basename(f, ".*")}
      subs = Submission.where(id: entries.keys.map(&:to_i), assignment_id: self.assignment_id).map{|s| [s.id, s]}.to_h
      entries.each do |sub_id, files|
        if files.length > 1
          ans[:errors] << "Submission #{sub_id} has multiple files in this archive; ignoring: #{files.map{|f| File.basename(f)}}"
          next
        elsif subs[sub_id.to_i].nil?
          ans[:errors] << "No such submission with id #{sub_id}"
          next
        else
          sub = subs[sub_id.to_i]
          sub_extracted_path = Upload.upload_path_for(sub.upload.extracted_path)
          file = files.first
          begin
            g = self.grade_for sub
            new_record = g.new_record?
            grader_dir = sub.upload.grader_path(g)
            grader_dir.mkpath
            if File.extname(file) == ".tap"
              raw_tap = File.read(file)
              if raw_tap == "DELETE"
                InlineComment.transaction do
                  InlineComment.where(submission: sub, grade: g).destroy_all
                  g.grading_output = nil
                  g.score = 0
                  g.available = false
                  g.out_of = self.avail_score
                  g.save!
                end
              else
                if block_given?
                  yield(g, raw_tap, sub)
                  File.open(grader_dir.join(File.basename(file)), "w") do |tap_out|
                    tap_out.write raw_tap
                    g.grading_output_path = tap_out.path
                  end
                  g.save!
                else
                  raw_tap = raw_tap.gsub(/(\s+filename:\s+\")/, "\\1#{sub_extracted_path}/")
                  tap = TapParser.new(raw_tap)
                  
                  File.open(grader_dir.join(File.basename(file)), "w") do |tap_out|
                    tap_out.write raw_tap
                    g.grading_output_path = tap_out.path
                  end
                  record_tap_as_comments(g, tap, sub)
                end
              end
            else
              contents = File.read(file)
              File.open(grader_dir.join(File.basename(file)), "w") do |out|
                out.write contents
                g.grading_output_path = out.path
              end
              if block_given?
                yield(g, contents, sub)
              else
                record_compile_error(sub, g)
              end
            end
            if new_record
              ans[:created] += 1
            else
              ans[:updated] += 1
            end
          rescue Exception => e
            ans[:errors] << "Error processing submission #{sub_id}: #{e} at #{e.backtrace}"
          end
        end
      end
    end
    {notice:
       if ans[:created] > 0 || ans[:updated] > 0
         "Created #{ans[:created]} #{"new grade".pluralize(ans[:created])}, and updated #{ans[:updated]} #{"existing grade".pluralize(ans[:updated])}"
       else
         nil
       end,
     errors:
       if ans[:errors].empty?
         nil
       else
         ("<p>#{"Problem".pluralize(ans[:errors].length)} updating #{"grade".pluralize(ans[:errors].length)}:</p>" +
          "<ul>" + ans[:errors].map{|msg| "<li>#{msg}</li>"}.join("\n") + "</ul>").html_safe
       end
    }
  end

  def dir_tree(path)
    tree = get_dir_tree(path)
    print_root("", tree.first[0], tree.first[1], [tree.first[0]]).join("\n")
  end
  def get_dir_tree(path)
    if path.file?
      [[path.basename.to_s, true]].to_h
    elsif path.directory?
      [[path.basename.to_s, path.children.map{|kid| get_dir_tree(kid)}.compact]].to_h
    else
      nil
    end
  end

  def print_root(indent, name, kids, lines)
    end_prefix = "└── "
    mid_prefix = "├── "
    end_spacer = "    "
    mid_spacer = "│   "
    if kids.is_a? Array
      kids.each_with_index do |kid, idx|
        is_last = (idx == kids.size - 1)
        if is_last
          kid.each do |name, kids|
            lines.push(indent + end_prefix + name)
            if kids.is_a? Array
              print_root(indent + end_spacer, name, kids, lines)
            end
          end
        else
          kid.each do |name, kids|
            lines.push(indent + mid_prefix + name)
            if kids.is_a? Array
              print_root(indent + mid_spacer, name, kids, lines)
            end
          end
        end
      end
    else
      lines.push(indent + name)
    end
    lines
  end
end
