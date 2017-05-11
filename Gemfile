# -*- ruby -*-
source 'https://rubygems.org'

gem 'rails', '~> 5.1'
gem 'rack'
gem 'i18n'

gem 'devise', github: 'plataformatec/devise' # FIXME: Remove github when devise updates.
gem 'erubis'
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
gem 'cocoon'

gem 'bootstrap-sass', '~> 3.3.5'
gem 'sass-rails', '>= 3.2'
gem 'bootstrap-sass-extras'
#gem 'bootstrap-datepicker-rails'
gem 'bootstrap3-datetimepicker-rails'
gem 'bootstrap-toggle-rails'

gem 'momentjs-rails', '>= 2.9.0'
gem 'font-awesome-rails'
gem 'zipruby-compat', :require => 'zipruby', :git => "https://github.com/jawspeak/zipruby-compatibility-with-rubyzip-fork.git", :tag => "v0.3.7" # needed instead of zipruby because write_xlsx needs rubyzip, and they conflict ; FIXME: git reference

gem 'delayed_job', github: 'dsander/delayed_job', branch: 'rails51' # FIXME: github
gem 'delayed_job_active_record', github: 'gogovan/delayed_job_active_record', branch: 'rails-5.1' # FIXME
gem 'daemons'

gem 'write_xlsx'

gem 'whenever', :require => false

gem 'pretender'

gem 'codemirror-rails'

group :development do
  #gem "flatten_migrations"
  gem "better_errors"
  gem "binding_of_caller"
  gem "listen"
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
  gem 'rails-controller-testing'
end
