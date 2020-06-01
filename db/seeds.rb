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
    name: "CS2500 Professor",
    first_name: "CS2500",
    last_name: "Professor",
    nickname: "cs2500 prof",
    email: "cs2500prof@localhost.localdomain",
    site_admin: false,
    username: "cs2500prof",
    nuid: 2345678,
    encrypted_password: Devise::Encryptor.digest(User, "cs2500prof")
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

  kyle = User.create(
    name: 'Kyle',
    first_name: 'Kyle',
    last_name: 'Sferrazza',
    nickname: 'kyle!',
    email: 'kyle@localhost.localdomain',
    site_admin: false,
    username: 'kyle',
    nuid: 999,
    encrypted_password: Devise::Encryptor.digest(User, 'kyle')
  )
  kyle.save!

  kyle_reg = Registration.create(
    course: fundies,
    user: kyle,
    role: 'student',
    show_in_lists: true
  )
  kyle_reg.save!

  RegistrationSection.create(
    registration: kyle_reg,
    section: lec
  ).save!
  RegistrationSection.create(
    registration: kyle_reg,
    section: lab1
  ).save!
  kyle.save!

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
  require 'etc'

  username = Etc.getlogin
  info = Etc.getpwnam(username)
  name = info.gecos.split(/,/).first

  name_split = name.split
  first_name = name_split[0]
  last_name = name_split[1]

  admin = User.create(
    name: name,
    first_name: first_name,
    last_name: last_name,
    site_admin: true,
    username: username,
  )
  admin.save!
end
