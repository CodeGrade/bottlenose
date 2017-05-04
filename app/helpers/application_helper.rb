# -*- coding: utf-8 -*-
module ApplicationHelper
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
end
