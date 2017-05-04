#!/usr/bin/env ruby

APP_PATH = File.expand_path('../../config/application',  __FILE__)
require File.expand_path('../../config/boot',  __FILE__)
require APP_PATH
Rails.application.require_environment!

nat = User.first_or_create(
  name:  "Nat Tuck",
  email: "nat@ferrus.net",
  site_admin: true,
)

puts "Admin user id: #{nat.id}"

term = Term.first_or_create(
  name: "Fall 2004",
)
term.save!

puts "Term Fall 2004 id: #{term.id}"

course = Course.first_or_create(
  name: "Spam Course",
  term: term,
)
course.save!

puts "Spam course id: #{course.id}"

bucket = Bucket.first_or_create(
  course: course,
  name: "Homework",
  weight: 1.0,
)

puts "Bucket id: #{bucket.id}"

hws = 0.upto(5).to_a.map do |ii|
  aa = Assignment.find_or_create_by(
    name: "HW #{ii}",
    course: course,
    bucket: bucket,
    blame_id: nat.id,
    due_date: (Time.now + 1.day),
  )
  aa.save!
  aa
end

regs = 0.upto(200).to_a.map do |ii|
   uu = User.find_or_create_by(
     name: "Sally#{ii} Student",
     email: "sally#{ii}@example.com",
   )
   uu.save!

   reg = Registration.find_or_create_by(
     course_id: course.id,
     user_id: uu.id,
     show_in_lists: true,
   )
   reg.save!
   reg
end

subs = []
hws.each do |hw|
  regs.each do |reg|
    0.upto(4) do |ii|
      sub = Submission.find_or_create_by(
        student_notes: "sub #{ii}",
        assignment_id: hw.id,
        user_id: reg.user.id,
        auto_score: 50 + rand(50),
        teacher_score: 50 + rand(50),
      )
      sub.save!
      subs << sub
    end
  end
end

puts "Created #{subs.count} subs."
