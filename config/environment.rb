# Load the Rails application.
require_relative 'application'

def fa_icon(name)
  %Q{<i class="fa fa-#{name}"></i>}.html_safe
end

# Initialize the Rails application.
Rails.application.initialize!
