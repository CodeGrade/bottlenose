class Audit
  @@log = Logger.new(File.open(Rails.root.join("log", "audit-#{Rails.env}.log"), 
                               File::WRONLY | File::APPEND | File::CREAT))
  def self.log(msg)
    @@log.info("#{Time.now}: #{msg}")
  end
end
