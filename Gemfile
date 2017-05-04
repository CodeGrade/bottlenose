# -*- ruby -*-
source 'https://rubygems.org'

gem 'rails', '~> 4.2'
gem 'rack'
gem 'i18n'

gem 'devise'
gem 'devise_ldap_authenticatable'

gem 'pg'

gem 'execjs'
gem 'therubyracer'

gem 'yaml_db'

gem 'activerecord-import'

gem 'coffee-rails'
gem 'coffee-rails-source-maps'
gem 'uglifier'
gem 'jquery-rails'
gem 'jquery-tablesorter'

gem 'bootstrap-sass', '~> 3.3.5'
gem 'sass-rails', '>= 3.2'
gem 'bootstrap-sass-extras'
gem 'bootstrap-datepicker-rails'
gem 'momentjs-rails', '>= 2.9.0'
gem 'bootstrap3-datetimepicker-rails', '~> 4.17.37'
gem 'font-awesome-rails'
gem 'zipruby-compat', :require => 'zipruby', :git => "https://github.com/jawspeak/zipruby-compatibility-with-rubyzip-fork.git", :tag => "v0.3.7" # needed instead of zipruby because write_xlsx needs rubyzip, and they conflict

gem 'delayed_job_active_record'
gem 'daemons'

gem 'write_xlsx'

gem 'whenever', :require => false

gem 'pretender'

gem 'codemirror-rails'

group :development do
  #gem "flatten_migrations"
  gem "better_errors"
  gem "binding_of_caller"
end

group :development, :test do
  gem 'pry'
  gem 'pry-rails'
end

group :test do
  gem 'simplecov'
  gem 'database_cleaner'
  gem 'capybara'
  gem 'capybara-webkit' # Needs qt5-default qt5-qmake libqt5webkit5-dev
  gem 'launchy'
  gem 'factory_girl_rails'
end
