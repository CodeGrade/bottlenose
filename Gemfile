# -*- ruby -*-
source 'https://rubygems.org'

gem 'rails', '~> 5.1'
gem 'rack'
gem 'i18n'

gem 'devise'
gem 'erubis'
gem 'devise_ldap_authenticatable' #, github: 'cschiewek/devise_ldap_authenticatable' # needed to use LDAP instead of email auth

gem 'pg'
gem "passenger", ">= 5.0.25", require: "phusion_passenger/rack_handler"

gem 'execjs'
gem 'therubyracer'

gem 'yaml_db' # used to dump database to YAML

gem 'activerecord-import' # used for bulk import of multiple InlineComments

gem 'coffee-rails'
gem 'coffee-rails-source-maps'
gem 'uglifier'
gem 'jquery-rails' # needed for general jQuery stuff
gem 'jquery-ui-rails' # needed specifically for drag-and-drop ability
gem 'jquery-tablesorter' # needed to allow re-sortable tables
gem 'cocoon' # used for dynamically generating nested forms

gem 'rubyzip' # used for reading submitted zip files

gem 'bootstrap-sass', '>= 3.4.1'
gem 'sass-rails', '>= 3.2'
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

group :development do
  #gem "flatten_migrations"
  gem "better_errors"
  gem "binding_of_caller"
  gem "listen"
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
  gem 'selenium-webdriver'
  gem 'webdrivers'
  gem 'launchy'
  gem 'factory_bot_rails'
  gem 'rails-controller-testing'
  gem 'single_test'
end
