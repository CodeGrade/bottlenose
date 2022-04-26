require_relative 'boot'

require 'rails/all'

# Require the gems listed in Gemfile, including any gems
# you've limited to :test, :development, or :production.
Bundler.require(*Rails.groups)

module Bottlenose
  class Application < Rails::Application
    # Initialize configuration defaults for originally generated Rails version.
    config.load_defaults 5.2

    # Settings in config/environments/* take precedence over those specified here.
    # Application configuration can go into files in config/initializers
    # -- all .rb files in that directory are automatically loaded after loading
    # the framework and any gems in your application.

    # Set Time.zone default to the specified zone and make Active Record auto-convert to this zone.
    # Run "rake -D time" for a list of tasks for finding time zone names. Default is UTC.
    config.time_zone = 'Eastern Time (US & Canada)'

    # The default locale is :en and all translations from config/locales/*.rb,yml are auto loaded.
    # config.i18n.load_path += Dir[Rails.root.join('my', 'locales', '*.{rb,yml}').to_s]
    # config.i18n.default_locale = :de

    config.autoload_paths << Rails.root.join('lib')
    config.assets.paths << Rails.root.join('node_modules')

    # Add subdirectories of models
    config.autoload_paths += Dir[Rails.root.join("app", "models", "{*/}")]

    config.assets.precompile += ["codemirror*", "codemirror/**/*",
                                 "pdfjs-dist*", "pdfjs-dist/**/*",
                                 "dompurify*", "dompurify/**/*"]

    config.active_job.queue_adapter = :backburner

    config.exceptions_app = self.routes

    config.action_controller.per_form_csrf_tokens = true

    config.to_prepare do
      # Only Applications list
      Doorkeeper::ApplicationsController.layout 'application'

      # Only Authorization endpoint
      Doorkeeper::AuthorizationsController.layout 'application'

      # Only Authorized Applications
      Doorkeeper::AuthorizedApplicationsController.layout 'application'
    end
  end
end
