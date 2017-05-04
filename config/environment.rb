# Load the Rails application.
require File.expand_path('../application', __FILE__)

def fa_icon(name)
  %Q{<i class="fa fa-#{name}"></i>}.html_safe
end

# Initialize the Rails application.
Rails.application.initialize!
