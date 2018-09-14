require 'fake_upload'

FactoryBot.define do
  sequence :user_name do |n|
    letters = ('A'..'Z').to_a
    first = letters[(n * 17) % 26] + "#{n}"
    last  = letters[(n * 13) % 26] + "#{n}"
    "#{first} #{last}"
  end

  factory :user do
    name  { generate(:user_name) }
    password { "password" }
    username { name.downcase.gsub(/\W/, '_') }
    email { username + "@example.com" }
    site_admin { false }

    factory :admin_user do
      site_admin { true }
    end
  end

  factory :term do
    semester { Term.semesters[:fall] }
    sequence(:year) {|n| Date.today.year + n}
    archived { false }
  end

  factory :course do
    term
    sequence(:name) {|n| "Computing #{n + 100}" }
    footer { "Link to Piazza: *Link*" }
    lateness_config

    after(:build) do |course|
      sec = build(:section, course: course)
      course.sections = [sec]
    end
  end

  factory :lateness_config do
    type { "LatePerDayConfig" }
    days_per_assignment { 365 }
    percent_off { 50 }
    frequency { 1 }
    max_penalty { 100 }
  end

  factory :section do
    course
    sequence(:crn) {|n| 1000 + n }
    sequence(:meeting_time) {|n| "Tuesday #{n}:00" }
    association :instructor, factory: :user
  end

  factory :grader do
    type { "ManualGrader" }
    sequence(:order)
    avail_score { 100.0 }
    params { "" }
    assignment
  end

  factory :teamset do
    course
    sequence(:name) {|n| "Default teamset #{n}"}
  end
  
  factory :assignment do
    type { "Files" }
    teamset
    association :blame, factory: :user
    
    lateness_config
    available { (Time.now - 10.days) }
    points_available { 100 }

    sequence(:name) {|n| "Homework #{n}" }
    due_date { (Time.now + 7.days) }
    after(:build) do |assn|
      assn.course = assn.teamset.course
      assn.graders << build(:grader, assignment: assn)
    end
  end

  factory :upload do
    user
    assignment

    file_name { "none" }
    secret_key { SecureRandom.hex }
    after(:build) do |u|
      u.upload_data = FakeUpload.new("none", "no content provided")
      u.metadata = {note: "Factory upload creation"}
    end
  end

  factory :submission do
    assignment
    user
    type { "FilesSub" }

    after(:build) do |sub|
      unless sub.user.registration_for(sub.course)
        create(:registration, user: sub.user, course: sub.course, role: 0, show_in_lists: true,
               new_sections: [sub.course.sections.first])
      end

      sub.upload_file = FakeUpload.new("none", "no content provided")
    end
  end

  factory :registration do
    user
    course

    role { Registration.roles[:student] }
    show_in_lists { true }
    after(:create) do |reg|
      reg.new_sections = [reg.course.sections.first]
      reg.save_sections
    end
  end

  factory :reg_request do
    user
    course

    notes { "Let me in!" }
    after(:build) do |reg|
      sections = {"#{reg.course.sections.first.type}_sections": "#{reg.course.sections.first.crn}"}
      reg.assign_attributes(sections)
    end
  end

  factory :team do
    teamset

    after(:build) do |team|
      team.course = team.teamset.course
      
      u1 = create(:user)
      u2 = create(:user)

      r1 = create(:registration, user: u1, course: team.course,
                  show_in_lists: true, new_sections: [team.course.sections.first])
      r1.save_sections
      r2 = create(:registration, user: u2, course: team.course,
                  show_in_lists: true, new_sections: [team.course.sections.first])
      r2.save_sections

      team.users = [u1, u2]
      team.start_date = Time.now - 2.days
    end
  end
end

