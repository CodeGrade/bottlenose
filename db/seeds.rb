require 'devise/encryptor'

[Course, Bucket, Assignment, Registration].each do |model|
  model.reset_column_information
end

def random(arr)
  arr[rand(arr.count)]
end

case Rails.env
when "development"
  nat = User.create!(
                     username: "nat",
                     name: "Nat Tuck",
                     first_name: "Nat",
                     last_name: "Tuck",
                     nickname: "Nat",
                     site_admin: true,
                     )

  nat.encrypted_password = Devise::Encryptor.digest(nat.class, "bacon88")
  nat.save!

  fixed_lateness = FixedDaysConfig.create!(days_per_assignment: 2)
  pct_lateness = LatePerDayConfig.create!(percent_off: 25, frequency: 1, days_per_assignment: 4, max_penalty: 100)
  
  # Create two terms.
  fall = Term.create!(name: "Fall 2015")
  spring = Term.create!(name: "Spring 2016")
when "production"
  nat = User.create!(
                     username: "ntuck",
                     name: "Nat Tuck",
                     first_name: "Nat",
                     last_name: "Tuck",
                     nickname: "Nat",
                     site_admin: true,
                     )
end
