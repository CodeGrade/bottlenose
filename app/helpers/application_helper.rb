# -*- coding: utf-8 -*-
require 'securerandom'

module ApplicationHelper
  def make_uuid
    SecureRandom.hex
  end

  def toggle_usernames_button(id, target, class: nil) 
    button_tag(type: nil, class: "btn btn-default no-resort", title: "Show/hide usernames", id: id, data: {target: target}) do
      content_tag(:span, nil, class: "glyphicon glyphicon-user")
    end 
   end

  def select_user_hash(users = nil)
    users ||= User.all
    hash = {}
    users.each do |user|
      hash[user.name] = user.id
    end
    hash
  end

  def select_course_hash
    hash = {}
    Course.all.each do |course|
      hash[course.name] = course.id
    end
    hash
  end

  def color_warning(score)
    if score >= 90
      return "gradeA"
    elsif score >= 80
      return "gradeB"
    elsif score >= 70
      return "gradeC"
    elsif score >= 60
      return "gradeD"
    else
      return "gradeF"
    end
  end
  
  def show_score(score, assignment = nil, admin = nil)
    assignment ||= @assignment

    if score.nil?
      score = "∅"
    end

    if admin.nil?
      admin = current_user.course_staff?(@course)
    end

    return to_fixed(score, 2) if assignment.nil?

    if assignment.hide_grading?
      if admin
        "(hidden #{to_fixed(score, 2)})"
      else
        "not ready"
      end
    else
      to_fixed(score, 2)
    end
  end

  def to_fixed(n, prec = 2)
    number_with_precision(n, :precision => prec)
  end
  
  def status_image(sub, grade_pct)
    if (sub.nil? || sub.new_record?)
      return image_tag("null-mark.png", height: 32)
    end

    if grade_pct.nil?
      # if sub.assignment.has_grading?
      if sub.created_at > (Time.now - 10.minutes)
        return image_tag("question-mark.png", height: 32)
      else
        return image_tag("null-mark.png", height: 32)
      end
    end

    if grade_pct >= 90.0
      return image_tag("check-plus.png", height: 32)
    elsif grade_pct >= 80
      return image_tag("check-mark.png", height: 32)
    elsif grade_pct >= 70
      return image_tag("c-mark.png", height: 32)
    elsif grade_pct >= 60
      return image_tag("cminus-mark.png", height: 32)
    elsif grade_pct >= 20
      return image_tag("sad-mark.png", height: 32)
    else
      return image_tag("cross-mark.png", height: 32)
    end
  end

  def user_image(user)
    image_path(Upload.upload_path_for(user.profile || 'silhouette.jpg'))
  end
  def user_link_data(user)
    {
      toggle: "tooltip",
      delay: {show: 0, hide: 250},
      title: "#{image_tag(user_image(user), alt: user.display_name, style: 'max-height: 300px; max-width: 300px;')}"
    }
  end
  def show_user(user)
    maybe_link_user(true, user)
  end
  def maybe_link_user(show, user, extra_class="")
    if show == true || (show.is_a?(Array) && show.member?(user.id))
      content_tag(:span,[
          link_to(user.display_name, user_path(user), class: "user-link #{extra_class}".strip, data: user_link_data(user)),
          content_tag(:span, "(#{user.username})", class: "username")
      ].flatten.join(" ").html_safe)
    else
      content_tag(:span,[
        content_tag(:span, user.name, class: "user-link #{extra_class}".strip, data: user_link_data(user)),
        content_tag(:span, "(#{user.username})", class: "username")
      ].flatten.join(" ").html_safe)
    end
  end

  def show_team(team)
    maybe_link_team(true, true, team)
  end
  def maybe_link_team(show_team, show_users, team, highlights={})
    content_tag(:span, [
                  if show_team || show_users
                    all_emails = team.users.map{|uu| "#{uu.name} <#{uu.email}>"}
                    mail_to all_emails.join(","), "@", title: "Email team members"
                  end,
                  if show_team
                    link_to("Team #{team.id}", course_teamset_team_path(@course, team.teamset, team))
                  else
                    "Team #{team.id}"
                  end,
                  " - ",
                  team.sorted_users.map do |u|
                    maybe_link_user(show_users, u, highlights[u.id]).html_safe
                  end.to_sentence
                ].flatten.join("\n").html_safe)
  end
  
  def code_textarea(str, options = {})
    # Within textareas, the initial newline is ignored
    # (https://w3c.github.io/html/syntax.html#restrictions-on-content-models)
    # so to handle files that start with whitespace, we have to be a bit careful.

    # The content_tag function will implicitly add a newline always, and using
    # a helper renderer function (as opposed to writing a <textarea> tag directly),
    # avoids any confusion of how to use it

    # The string will be rendered to html (to escape any special characters),
    # unless it's already been explicitly marked as html_safe.
    str = render(html: str) unless str.html_safe?
    content_tag(:textarea, str, options)
  end

  def registration_show_toggle_path(reg_id)
    "/registrations/#{reg_id}/toggle_show"
  end

  def new_chapter_assignment_path(ch)
    new_course_assignment_path(ch.course) + "?chapter_id=#{ch.id}"
  end

  def grading_drivers
    Dir.entries(Rails.root.join('sandbox', 'drivers')).find_all do |ent|
      ent =~ /\.rb$/
    end
  end

  def sanitize_question(html, options = {})
    options[:tags] = %w(p ol ul li b strong i em textarea code pre) unless options[:tags]
    if options[:allow_code]
      options.delete(:allow_code)
      attrs = (options[:attributes] || []) + %w"class data-lang"
      options[:attributes] = attrs
    end
    sanitize(html, options)
  end

  def self.capture3(*cmd, stdin_data: '', binmode: false, timeout: nil, signal: :TERM, **opts)
    if opts[:chdir].nil?
      opts.delete :chdir
    end
    Open3.popen3(*cmd, opts) do |i, o, e, t|
      if binmode
        i.binmode
        o.binmode
        e.binmode
      end
      out_reader = Thread.new { o.read }
      err_reader = Thread.new { e.read }
      begin
        i.write stdin_data
      rescue Errno::EPIPE
      end
      i.close
      timed_out = false
      if timeout
        if !t.join(timeout)
          timed_out = true
          Process.kill(signal, t.pid)
          # t.value below will implicitly .wait on the process
        end
      end
      [out_reader.value, err_reader.value, t.value, timed_out]
    end
  end

  def self.mime_type(full_path)
    case File.extname(full_path).downcase
    when ".java"
      "text/x-java"
    when ".class"
      "application/java-vm"
    when ".js"
      "text/javascript"
    when ".arr"
      "pyret"
    when ".rkt", ".ss"
      "scheme"
    when ".lisp"
      "text/x-common-lisp"
    when ".sml", ".ml", ".mli", ".mll"
      "mllike"
    when ".lean"
      "text/lean"
    when ".hs"
      "text/x-haskell"
    when ".lhs"
      "text/x-literate-haskell"
    when ".md"
      "text/markdown"
    when ".mly"
      "text/x-ebnf"
    when ".c", ".h"
      "text/x-csrc"
    when ".cpp", ".c++"
      "text/x-c++src"
    when ".cs"
      "text/x-csharp"
    when ".py"
      "text/x-python"
    when ".gif"
      "image/gif"
    when ".jpg", ".jpeg"
      "image/jpeg"
    when ".png"
      "image/png"
    when ".tiff"
      "image/tiff"
    when ".webp"
      "image/webp"
    when ".yaml"
      "text/x-yaml"
    when ".jar"
      "jar"
    when ".zip"
      "zip"
    when ".7z"
      "application/x-7z-compressed"
    when ".svg", ".xml"
      "application/xml"
    when ".html"
      "text/html"
    when ".css"
      "text/css"
    when ".tex", ".sty"
      "text/x-latex"
    when ".bib"
      "text/plain"
    when ".log", ".tap", ".txt", ".text"
      "text/plain"
    when ".pdf"
      "application/pdf"
    when ".rtf"
      "application/rtf"
    when ".mp3"
      "audio/mpeg"
    when ".csv"
      "text/csv"
    else
      case File.basename(full_path.to_s).downcase
      when "makefile"
        "text/x-makefile"
      when "readme"
        "text/plain"
      else
        "text/unknown"
      end
    end
  end
  def self.binary?(mimetype)
    # NOTE: The mimetypes here must match the ones produced by mime_type above
    # NOTE: text/unknown is treated as binary, so that (a) browsers won't try to execute it,
    # but (b) it won't be forced to UTF-8 in Submission#get_submission_files
    case mimetype
    when "text/x-java",
         "text/javascript",
         "pyret",
         "scheme",
         "text/x-common-lisp",
         "mllike",
         "text/lean",
         "text/x-haskell",
         "text/x-literate-haskell",
         "text/markdown",
         "text/x-ebnf",
         "text/x-csrc",
         "text/x-c++src",
         "text/x-csharp",
         "text/x-python",
         "application/xml",
         "text/x-yaml",
         "text/html",
         "text/css",
         "text/x-latex",
         "text/plain",
         "text/x-makefile",
         "text/csv"
      false
    when "application/java-vm",
         "image/gif",
         "image/jpeg",
         "image/png",
         "image/tiff",
         "image/webp",
         "jar",
         "zip",
         "application/x-7z-compressed",
         "application/pdf",
         "application/rtf",
         "audio/mpeg",
         "text/unknown"
      true
    else
      true
    end
  end

    
  def self.make_yaml_safe(val)
    if val.is_a? String
      val = String.new(val).force_encoding(Encoding::UTF_8)
      val.scrub do |bytes|
        "\u27e6#{bytes.unpack("H*")[0]}\u27e7" # square double brackets for invalid chars
      end.gsub(/[^\p{print}\p{space}]/) do |match|
        "\u27ea#{match.unpack("H*")[0]}\u27eb" # angled double brackets for unprintable chars
      end
    elsif val.is_a? Array
      val.map do |v| make_yaml_safe(v) end
    elsif val.is_a? Hash
      val.map do |k, v| [make_yaml_safe(k), make_yaml_safe(v)] end.to_h
    else
      val
    end
  end
end
