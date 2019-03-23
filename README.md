# Bottlenose &middot; [![Build Status](https://travis-ci.org/CodeGrade/bottlenose.svg?branch=master)](https://travis-ci.org/CodeGrade/bottlenose)

Bottlenose is a web app for managing assignment submission and grading in
computer science courses. It provides a flexible mechanism for automatic grading
of programming assignments.

## Development Environment

Bottlenose is built expecting the following environment:

 * Ubuntu 16.04 
 * A BTRFS (or ZFS) filesystem for at least /var (although a btrfs root is easiest)
 * PostgreSQL
 * Ruby + Bundler
 * Nodejs + npm

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
sudo apt-get install build-essential git postgresql libpq-dev beanstalkd imagemagick
```

After installing beanstalkd, double check that "telnet [server] 11300" doesn't
work from an external machine. Otherwise you got your firewall wrong.

Some of the autograders (e.g. Racket) require a synthetic X connection; for
that, Bottlenose uses xvfb:

```sh
sudo apt-get install xvfb
```

The server will also support converting .rtf files to .pdf files,
using LibreOffice:

```sh
sudo apt-get install libreoffice-core libreoffice-base
```

Make certain that the following programs are available in your path
(configured in whatever login shell the `bottlenose` user uses):

 * `javac` and `java` (needed for Java-related graders)
 * `xvfb-run` and `racket` (needed for Racket autograder)
 * `ip`, `ls`, `hostname` (needed for general configuration)
 * `convert` (from imagemagick, needed for profile photos)
 * `soffice` (from libreoffice, needed to convert .rtf to .pdf)

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
npm install
```

### Running Bottlenose in Development

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

### Running Bottlenose in Production
Configure nginx with Passenger:
```
server {
        listen 443 ssl;
        listen [::]:443 ssl;

        server_name <your server name here, e.g. handins.edu>;
        ssl_certificate     /path/to/your/server-cert.cer;
        ssl_certificate_key /path/to/your/private-key.pem;
        ssl_protocols TLSv1 TLSv1.1 TLSv1.2;
        client_max_body_size 10M;

        root /path/to/bottlenose/src/public;

        # First attempt to serve request as file, then
        # as directory, then fall back to displaying a 404.
        try_files $uri @passenger;

        error_page 502 /502.html;

        location @passenger {
            # logic adapted from https://lincolnloop.com/blog/pro-tip-redirecting-custom-nginx-maintenance-page/
            if (-f /path/to/bottlenose/src/public/maintenance.html) {
                return 503;
            }
            passenger_enabled on;
            passenger_ruby /path/to/bottlenose/.rbenv/versions/2.4.1/bin/ruby;
            passenger_sticky_sessions on;
            rails_env production;
        }

        location = /502.html {
            internal;
            root /path/to/bottlenose/src/public/;
        }
        error_page 503 @maintenance;
        location @maintenance {
            root /path/to/bottlenose/src/public/;
            rewrite ^(.*)$ /maintenance.html break;
        }
}
```

You can then restart the server with

```sh
(cd /path/to/bottlenose/src ; bundle exec passenger-config restart-app /path/to/bottlenose/src)
```

and you can start or restart the background graders with

```sh
(cd /path/to/bottlenose/src ; screen -S backburner -X quit ; screen -d -m -S backburner env RAILS_ENV=production bundle exec backburner -l log/backburner.log)
```
Once the server has been started, go to the Settings > Edit page,
ensure that the Site URL is filled in, and click Save Settings.
