
require 'open3'
require 'tempfile'

class Container
  def initialize(name)
    @name = name
  end

  def start!
    run(
      %Q{lxc launch bn-base "#{@name}" -e \
           -c "limits.cpu.allowance=20ms/60ms" \
           -c "limits.memory=512MB" \
           -c "limits.processes=128"}
    )

    loop do
      sleep(1)
      state = `(lxc exec #{@name} -- runlevel) 2>&1`
      break if state =~ /^N/;
    end

    run("lxc exec #{@name} -- hostname #{@name}")
    run("lxc exec #{@name} -- bash -c 'echo 127.0.0.1 $(hostname) >> /etc/hosts'")
  end

  def mkdir(path, mode = 0755)
    run(%Q{lxc exec #{@name} -- mkdir -m 0#{mode.to_s(8)} -p "#{path}"})
  end

  def push(src, dst)
    run(%Q{lxc file push "#{src}" "#{@name}/#{dst}"})
  end

  def push_dir(src, dst)
    src_parent = File.dirname(src)
    src_name   = File.basename(src)
    run(%Q{(cd "#{src_parent}" && tar cf - "#{src_name}") | \
        lxc exec #{@name} -- bash -c '(cd "#{dst}" && tar xf -)'})
  end

  def pull(src, dst)
    raise Exception.new("TODO")
  end

  def chmod(path, mode)
    run(%Q{lxc exec #{@name} -- chmod 0#{mode.to_s(8)} "#{path}"})
  end

  def chmod_r(path, mode)
    run(%Q{lxc exec #{@name} -- chmod -R 0#{mode.to_s(8)} "#{path}"})
  end

  def exec_driver(secret, sub_up, gra_up, xtr_up = nil)
    puts "XX In exec_driver"

    FileUtils.mkdir_p("/tmp/bn")
    Tempfile.create("driver.", "/tmp/bn") do |drv|
      puts "XX in block"

      tmpl = Rails.root.join("sandbox", "driver.pl.erb")
      erb  = Erubis::Eruby.new(File.read(tmpl))
      erb.filename = tmpl.to_s

      puts "XX in block 2"
      data = {
        cookie: secret,
        timeout: 60, # Timeout is per step, e.g. unpack, build, test
        sub_url: sub_up.url,
        sub_name: sub_up.file_name,
        gra_url: gra_up.url,
        gra_name: gra_up.file_name,
        xtr_url: nil,
        xtr_name: nil,
      }

      if xtr_up
        data[:xtr_url]  = ext_up.url
        data[:xtr_name] = ext_up.file_name
      end
      puts "XX in block 4"

      driver_text = erb.result(data)
      puts "XX driver:\n#{driver_text}"

      drv.write(driver_text)
      drv.close

      puts "XX Tempfile path: #{drv.path}"

      push(drv.path, "/root/driver.pl")
    end

    command = %Q{lxc exec "#{@name}" -- bash -c "perl /root/driver.pl"}
    puts "XX Run driver: #{command}"

    Thread.new do
      sleep 10.minutes
      force_stop!
    end

    Open3.capture3(command)
  end

  def stop!
    puts "Stopping #{@name}"
    run(%Q{lxc stop #{@name} --timeout 5})
    sleep(1)
    force_stop!
  end

  def force_stop!
    puts "Force stopping #{@name}"
    system(%Q{(lxc stop "#{@name}" --force 2>&1) > /dev/null})
  end

  private

  def run(cmd)
    #puts "Run: #{cmd}"
    system(cmd) or begin
      puts "Error running command: #{cmd}"
      force_stop!
      raise Exception.new("Error running command: #{cmd}")
    end
  end
end

