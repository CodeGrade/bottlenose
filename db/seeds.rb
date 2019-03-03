case Rails.env
when "development"
  admin = User.create(
    name: "Site Administrator",
    first_name: "Site",
    last_name: "Administrator",
    nickname: "Admin",
    email: "admin@example.com",
    site_admin: true,
    username: "admin",
    nuid: 1234567,
    encrypted_password: Devise::Encryptor.digest(User, "admin")
  )
  admin.save!

  prof = User.create(
    name: "Doctor Professor",
    first_name: "Doctor",
    last_name: "Professor",
    nickname: "Dr. P",
    email: "prof@example.com",
    site_admin: false,
    username: "prof",
    nuid: 2345678,
    encrypted_password: Devise::Encryptor.digest(User, "prof")
  )
  prof.save!

  f18 = Term.create(semester: "fall", year: 2018)
  f18.save!

  lc = LatenessConfig.create(
    type: "LatePerDayConfig",
    days_per_assignment: 4,
    percent_off: 25,
    frequency: 1,
    max_penalty: 100)
  lc.save!

  fundies = Course.new(
    term: f18,
    name: "CS2500",
    footer: "",
    lateness_config: lc)

  lec = Section.new(
    course: fundies,
    crn: 0,
    meeting_time: "MWR 10:30am",
    instructor: prof,
    type: "lecture"
  )

  lab1 = Section.new(
    course: fundies,
    crn: 1,
    meeting_time: "F 1:35pm",
    instructor: prof,
    type: "lab"
  )

  lab2 = Section.new(
    course: fundies,
    crn: 2,
    meeting_time: "F 3:25pm",
    instructor: prof,
    type: "lab"
  )

  fundies.sections = [lec, lab1, lab2]

  fundies.save!

  for i in 0..39
    lab = i < 20 ? lab1 : lab2
    num = i.to_s.rjust(2, "0")
    user = User.create(
      name: "User #{num}",
      first_name: "User",
      last_name: num,
      nickname: "Nickname #{num}",
      email: "user#{i}@example.com",
      site_admin: false,
      username: "user#{num}",
      nuid: 1000000 + i,
      encrypted_password: Devise::Encryptor.digest(User, "user#{num}"),
    )
    reg = Registration.create(
      course: fundies,
      user: user,
      role: "student",
      show_in_lists: false,
    )
    reg.save!
    RegistrationSection.create(
      registration: reg,
      section: lec
    ).save!
    RegistrationSection.create(
      registration: reg,
      section: lab
    ).save!
    user.save!
  end
when "production"
  # create an admin user based on username
end
