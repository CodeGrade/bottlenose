require_relative "boot"

require "rails/all"

# Require the gems listed in Gemfile, including any gems
# you've limited to :test, :development, or :production.
Bundler.require(*Rails.groups)

module Bottlenose
  class Application < Rails::Application
    # Initialize configuration defaults for originally generated Rails version.
    config.load_defaults 7.0

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

    # Configuration for the application, engines, and railties goes here.
    #
    # These settings can be overridden in specific environments using the files
    # in config/environments, which are processed later.
    #
    config.time_zone = "Eastern Time (US & Canada)"
    # config.eager_load_paths << Rails.root.join("extras")
  end
end
