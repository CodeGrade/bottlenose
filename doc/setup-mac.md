# Setup Bottlenose on mac

Note: for settingup Bottlenose on *Ubuntu*, please go here: [README.md](https://github.com/CodeGrade/bottlenose/blob/master/README.md)

## Development Environment

Bottlenose is built expecting the following environment:

  * macOS
  * A BTRFS (or ZFS) filesystem for /var (if possible)
  * PostgreSQL
  * Ruby + Bundler
  * Homebrew
  * Beanstalk
  
### Homebrew

```	
# Install Homebrew
$/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

# Install postgres, npm, beanstalk, iproute
brew install postgresql node beanstalk iproute2mac
```

### Postgres

Start postgres:

```sh
brew services start postgresql
```

Start beanstalk:

```sh
brew services start beanstalk
```


Create a user

```sh
# Create the bottlenose role for postgres.
createuser -d bottlenose
```

If you run into authentication trouble later, you may need to modify
the pg_hba.conf in /etc/postgres/.../ to allow local ident auth.

### Ruby

Best practice for Ruby in development is to use a version manager like rvm or rbenv. Once you have the correct version [installed] (https://github.com/rbenv/rbenv#homebrew-on-macos), e.g., by doing something like this:

```
brew install rbenv ruby-build
rbenv init # follow instructions to make this persistent
rbenv install
```


```
# Install Ruby's package manager "Bundler".
gem install bundler

# Install Bottlenose's dependencies.
# (from the bottlenose directory checked out from git)
bundle install
```

### Javascript

```
npm install
```
 
### Running Bottlenose

Bottlenose is mostly a standard Rails app.

Currently the bigest weirdness is the use of delayed job.

```sh
# Get a fresh database
bundle exec rails db:create
bundle exec rails db:migrate

# Start background job worker
bundle exec rake backburner:work

# Start the server
bundle exect rails s

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

