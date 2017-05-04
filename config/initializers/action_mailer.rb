
# Set default hostname for emailed URLs.
unless Rails.env.test?
  ActionMailer::Base.default_url_options[:host] = `hostname -f`.chomp
  ActionMailer::Base.delivery_method = :sendmail
end
