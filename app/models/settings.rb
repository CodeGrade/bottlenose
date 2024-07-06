class Settings
  def self.defaults
    {
      "site_email"   => 'Bottlenose <noreply@example.com>',
      "backup_login" => '',
      "site_url"     => '',
    }
  end

  def self.set_site_url!(req)
    cfg = load_json
    cfg["site_url"] = "#{req.protocol}#{host_primary_ip}:#{req.port}"
    save_json(cfg)
  end

  def self.host_primary_ip
    route = `ip route get 8.8.8.8`
    iface = route.match(/dev\s+(\w+?)\s/)[1]
    addrs = `ip a show dev "#{iface}"`
    addrs.match(/inet\s+(\d+\.\d+\.\d+\.\d+)[\s|\/]/)[1]
  end

  def self.clear_test!
    FileUtils.rm(File.expand_path("~/.config/bottlenose/test.json"), force: true)
  end

  def self.file_path
    Pathname.new(File.expand_path("~/.config/bottlenose/#{Rails.env}.json"))
  end

  def self.load_json
    unless File.exist?(file_path)
      return defaults
    end

    defaults.merge(JSON.parse(File.read(file_path)))
  end

  def self.save_json(cfg)
    FileUtils.mkdir_p(file_path.parent)
    File.write(file_path, cfg.to_json)
  end

  def self.[](key)
    cfg = load_json
    return cfg[key]
  end
end
