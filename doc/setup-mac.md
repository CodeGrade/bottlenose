# Setup Bottlenose on mac

Note: for settingup Bottlenose on *Ubuntu*, please go here: [README.md](https://github.com/CodeGrade/bottlenose/blob/master/README.md)

## Development Environment

Bottlenose is built expecting the following environment:

  * macOS
  * A BTRFS (or ZFS) filesystem for /var (if possible)
  * PostgreSQL
  * Ruby + Bundler
  * Homebrew
  
### Homebrew

```	
# Install Homebrew
$/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

# Install postgres, Ruby, and npm 
brew install postgresql node
```

### Postgres

```sh
# Switch to the postgres user to create the new bottlenose role.
sudo su - postgres

# Create the bottlenose role for postgres.
createuser -d bottlenose

# Exit back to the vagrant user.
exit
```

If you run into authentication trouble later, you may need to modify
the pg_hba.conf in /etc/postgres/.../ to allow local ident auth.

### Ruby

Best practice for Ruby in development is to use a version manager like rvm or rbenv. Once you have the correct version [installed] (https://github.com/rbenv/rbenv#homebrew-on-macos)
```
# Install Ruby's package manager "Bundler".
gem install bundler

# Install Bottlenose's dependencies.
# (from the bottlenose directory checked out from git)
bundle install
```
### Capybara-webkit

**Prerequisite: Xcode (between 9.4 - 10.1).** Newer versions of Xcode will not work and you will have to downgrade! To do this follow these [instructions] (https://medium.com/@tseboho/how-to-downgrade-xcode-4359df5158d5)

**NOTE:** 

Qt 5.5 is the last version of Qt that capybara-webkit will support. The Qt project has dropped the WebKit bindings from binary releases in 5.6. Make sure that you have Xcode installed (between 9.4 - 10.1). Under Xcode preferences locations, make sure there is a version set. Qt 5.5 was removed from homebrew in newer commits. The previous/parent commit was **9ba3d6e**. So, to be able to install Qt 5.5 with homebrew checkout the old commit first:

```
brew update
cd $( brew --prefix )/Homebrew/Library/Taps/homebrew/homebrew-core
git checkout 9ba3d6ef8891e5c15dbdc9333f857b13711d4e97 Formula/qt@5.5.rb
brew install qt@5.5
```
**NOTE:**  

- If you get Error fatal: reference is not a tree: 9ba3d6ef8891e5c15dbdc9333f857b13711d4e97, use git fetch --unshallow to complete git history.
- If you get Error: qt@5.5: unknown version :mountain_lion or :mojave, comment out line #25 in Formula/qt@5.5.rb

The Homebrew formula for qt@5.5 is keg only which means binaries like qmake will not be symlinked into your /usr/local/bin directory and therefore will not be available for capybara-webkit.
```
# Then add to your shell configuration file:
echo 'export PATH="$(brew --prefix qt@5.5)/bin:$PATH"' >> ~/.bashrc	

# Now you can install capybara-webkit
 gem install capybara-webkit
```
 
### Running Bottlenose

Bottlenose is mostly a standard Rails app.

Currently the bigest weirdness is the use of delayed job.

```sh
# Get a fresh database
rails db:create
rails db:migrate

# Start background job worker
rake backburner:work

# Start the server
rails s

#If it returns an error saying it cannot find rails use
script/rails s

# App should be at localhost:3000, as per usual.
```

### Troubleshooting

When you run `rails db:create`, and if you get the following error message:

```sh
FATAL:  role "bottlenose" does not exist
Couldn't create database for {"adapter"=>"postgresql", "encoding"=>"unicode", "database"=>"bottlenose_development", "pool"=>5, "username"=>"bottlenose", "password"=>nil}
rake aborted!
ActiveRecord::NoDatabaseError: FATAL:  role "bottlenose" does not exist
```

You will need to create the user (AKA "role") inside PostgreSQL using psql.exe:

```sh
# Start psql.exe
$ psql -d postgres

# Create the user: bottlenose (this should be the application name)
postgres=# create role bottlenose login createdb;

# Quit psql.exe
postgres=# \q
```

Expect terminal to return:

```sh
Created database 'bottlenose_development'
Created database 'bottlenose_test'
```

Then continue by running `rails db:create`.



**Problem 2**:

If you have successfully started rails server, but recevied this message when you type `localhost:3000` in your browser:

```sh
PG::ConnectionBad at /users/sign_in
could not connect to server: No such file or directory
Is the server running locally and accepting
connections on Unix domain socket "/tmp/.s.PGSQL.5432"?
```

You need to start postgres server manually, type the following in your terminal:

```
pg_ctl -D /usr/local/var/postgres -l /usr/local/var/postgres/server.log start

```

For stoping the server manually, type:

```
pg_ctl -D /usr/local/var/postgres stop -s -m fast

```

