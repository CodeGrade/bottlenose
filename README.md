# Bottlenose

Bottlenose is a web app for managing assignment submission and grading in
computer science courses. It provides a flexible mechanism for automatic grading
of programming assignments.

## Development Environment

Bottlenose is built expecting the following environment:

 * Ubuntu 16.04 
 * A BTRFS (or ZFS) filesystem for at least /var (although a btrfs root is easiest)
 * PostgreSQL
 * Ruby + Bundler

For deployment, Phusion Passenger + Nginx is recommended.

Notes for setting up Bottlenose dev on Mac, please go here: [setup_mac](../master/doc/setup-mac.md)

## Bottlenose Setup

### Basics

First, make sure you have an active software firewall. Beanstalkd is a network service
with no authentication, so you want to make sure its port is blocked.

```sh
sudo ufw allow 22/tcp
sudo ufw allow 80/tcp
sudo ufw allow 443/tcp
# ... any other external incoming ports
sudo ufw enable
```

Some packages are generally good to have, and needed by many future steps in
the setup process.

```sh
sudo apt-get install build-essential git postgresql libpq-dev beanstalkd
```

After installing beanstalkd, double check that "telnet [server] 11300" doesn't
work from an external machine. Otherwise you got your firewall wrong.

Some of the autograders (e.g. Racket) require a synthetic X connection; for
that, Bottlenose uses xvfb:

```sh
sudo apt-get install xvfb
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

### Database startup for a dev env is:

* As user "postgres":
  ```
   createuser -d bottlenose
  ```
 * As the dev user:
  ```
   rake db:create
   rake db:schema:load
   rake db:seed
  ```

* If you want to start clean on an existing Bottlenose, there's a helper to drop/create/load/seed:
  ```
   rake db:nuke
  ```

### Ruby

Best practice for Ruby in development is to use a version manager like
[rvm](http://rvm.io) or rbenv. Once you have the correct version installed,

```sh
# Install Ruby's package manager "Bundler".
gem install bundler

# Install Bottlenose's dependencies.
# (from the bottlenose directory checked out from git)
bundle install
```

### Running Bottlenose

Bottlenose is mostly a standard Rails app.

```sh
# Make sure you've got the database set up.

# Start background task worker
# This'll need a dedicated terminal
rake backburner:work

# Start the server
# This'll also need a dedicated terminal
rails s

# App should be at localhost:3000, as per usual.
```

