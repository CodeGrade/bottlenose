require 'open3'
require 'digest'
require 'fileutils'

class Steno
  attr_accessor :sub

  def initialize
    $stdout.sync = true
    $stderr.sync = true

    @cookie  = ENV.delete("COOKIE")  || "COOKIE"
    @sub     = ENV.delete("SUB") or raise Exception.new("Must set SUB env var")
    @timeout = (ENV.delete("TIMEOUT") || 60).to_i
    @ktime   = @timeout + 10
    @hashes  = {}
  end

  def hash_file(path)
    Digest::SHA256.digest(File.read(path))
  end

  def save_grading_hashes(xs)
    xs.each do |name|
      hash = hash_file("_grading/#{name}")
      @hashes[name] = hash
    end
  end

  def check_grading_hashes
    @hashes.each do |name, hash0|
      hash1 = hash_file("_grading/#{name}")
      if hash0 != hash1
        puts "Hash mismatch for _grading/#{name}"
        puts "Hax!"
        exit(1)
      end
    end
  end

  def shell(cmd)
    cmd.gsub!(%q{"}, %q{\"})
    puts cmd
    system(%Q{timeout -k #{@ktime} #{@timeout} bash -c "#{cmd}"}) or
      raise Exception.new("shell(#{cmd}) -> #{$?}")
  end

  def unpack
    if @sub =~ /\.zip$/i
      shell(%Q{unzip '#{@sub}'})
    end
    if @sub =~ /\.tar$/i
      shell(%Q{tar xf '#{@sub}'})
    end
    if @sub =~ /\.tar.gz$/i || @sub =~ /\.tgz$/i
      shell(%Q{tar xzf '#{@sub}'})
    end

    items = Dir.entries(".").reject do |name|
      name =~ /^\./ || name == "_grading" || !File.directory?(name)
    end

    if items.size == 1 && File.directory?(items[0])
      dirname = items[0]
      shell(%Q{mv #{dirname}/* .})
      shell(%Q{mv #{dirname} #{dirname}.#{$$}})
    end
  end

  def run_tests(cmd)
    output = ""
    errors = ""

    cmd.gsub!(%q{"}, %q{\"})

    Open3.popen3(%Q{timeout -k #{@ktime} #{@timeout} bash -c "#{cmd}"}) do |_in, out, err, _wait|
      loop do
        begin
          unless out.eof?
            tmp = out.read_nonblock(256)
            output << tmp
            $stdout.write(tmp)
          end
          unless err.eof?
            tmp = err.read_nonblock(256)
            errors << tmp
            $stderr.write(tmp)
          end
        rescue IO::WaitReadable
          IO.select([out, err], nil, nil, 1.0)
        end

        break if out.eof? && err.eof?
      end
    end

    puts "\n\n"
    puts @cookie
    puts output
    puts @cookie
    puts "\n== Stderr ==\n"
    puts errors
  end
end

