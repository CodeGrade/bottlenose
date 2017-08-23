Backburner.configure do |config|
  config.beanstalk_url       = "beanstalk://127.0.0.1"
  config.tube_namespace      = "bottlenose.#{Rails.env}"
  config.namespace_separator = "."
  config.on_error            = lambda { |e| puts e }
  config.max_job_retries     = 1 # default 0 retries
  config.retry_delay         = 10 # default 5 seconds
  config.retry_delay_proc    = lambda { |min_retry_delay, num_retries| min_retry_delay + (num_retries ** 3) }
  config.default_priority    = 1024
  config.respond_timeout     = 240
  config.default_worker      = Backburner::Workers::Forking
  config.logger              = Logger.new(STDOUT)
  config.primary_queue       = "backburner-jobs"
  config.priority_labels     = { :high => 50, :low => 1000 }
  config.reserve_timeout     = nil
  config.job_serializer_proc = lambda { |body| JSON.dump(body) }
  config.job_parser_proc     = lambda { |body| JSON.parse(body) }
end
