# frozen_string_literal: true

namespace :factory_bot do
  desc 'Verify that all FactoryBot factories are valid'
  task lint: :environment do
    require 'factory_bot_rails'
    if Rails.env.test?
      conn = ActiveRecord::Base.connection
      conn.transaction do
        FactoryBot.lint
        raise ActiveRecord::Rollback
      end
    else
      system("bundle exec rake factory_bot:lint RAILS_ENV='test'")
      fail if $?.exitstatus.nonzero?
    end
  end
end
