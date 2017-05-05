# Run me with "rails runner db/simple-data.rb"

require 'devise/encryptor'

def create_user(name, password)
  first, last = name.split(/\s+/)

  uu = User.create!(
    username: first.downcase,
    name: name,
    first_name: first,
    last_name: last,
    nickname: first,
    site_admin: false,
  )

  uu.encrypted_password = Devise::Encryptor.digest(uu.class, password)
  uu.save!
end

create_user("Alice Anderson", "alice88")
create_user("Bob Baker", "bob88")
create_user("Carol Cooper", "carol88")
create_user("Dave Dyson", "dave88")

