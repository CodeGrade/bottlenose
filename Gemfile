# -*- ruby -*-
source 'https://rubygems.org'

gem 'rails', '~> 7.0'
gem 'rack'
gem 'i18n'

gem 'webpacker'

gem 'devise'
gem 'erubis'
gem 'devise_ldap_authenticatable' #, github: 'cschiewek/devise_ldap_authenticatable' # needed to use LDAP instead of email auth

gem 'pg'
gem "passenger", ">= 5.0.25", require: "phusion_passenger/rack_handler"

gem 'execjs'
gem 'therubyracer'

gem 'yaml_db' # used to dump database to YAML

gem 'activerecord-import' # used for bulk import of multiple InlineComments
gem 'sprockets', '~>3.7'

gem 'coffee-rails'
gem 'coffee-rails-source-maps'
gem 'uglifier'
gem 'jquery-rails' # needed for general jQuery stuff
gem 'jquery-ui-rails' # needed specifically for drag-and-drop ability
gem 'jquery-tablesorter' # needed to allow re-sortable tables
gem 'cocoon' # used for dynamically generating nested forms

gem 'rubyzip' # used for reading submitted zip files

gem 'bootsnap', '>= 1.1.0', require: false
gem 'bootstrap-sass', '>= 3.4.1'
gem 'sassc-rails'
gem 'bootstrap-sass-extras'
gem 'bootstrap3-datetimepicker-rails' # for the datetime widget
gem 'bootstrap-toggle-rails' # for toggle buttons instead of checkboxes

gem 'momentjs-rails', '>= 2.9.0' # needed for human-friendly textual dates
gem 'font-awesome-rails'

gem 'addressable'

gem 'kramdown' # needed for markdown support
gem 'kramdown-parser-gfm' # needed for Github-flavored markdown, as of kramdown 2.0.0
gem 'rouge'
#gem 'kramdown-syntax-coderay' # support for syntax highlighting, as of kramdown 2.0.0

# needed to run graders in the background
gem 'backburner'
gem 'beaneater'
gem 'daemons'

gem 'headless' # needed to run Racket from BN, and xvfb-run (the shell script) merges stdout and stderr

gem 'write_xlsx' # needed to export grade spreadsheet

gem 'whenever', :require => false

gem 'pretender'

gem 'doorkeeper', "5.4.0"

group :development do
  #gem "flatten_migrations"
  gem "better_errors"
  gem "binding_of_caller"
  gem "listen"
  gem "ruby-debug-ide"
  gem "debase"
  gem "bundler-audit"
end

group :development, :test do
  gem 'puma'
  gem 'pry'
  gem 'pry-rails'
  gem 'minitest-reporters'
end

group :test do
  gem 'simplecov'
  gem 'database_cleaner'
  gem 'capybara'
  gem 'capybara-webkit' # Needs qt5-default qt5-qmake libqt5webkit5-dev
  gem 'webdrivers', '~> 4.0'
  gem 'launchy'
  gem 'factory_bot_rails'
  gem 'rails-controller-testing'
  gem 'single_test'
end
