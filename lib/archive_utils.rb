require 'audit'
require 'rubygems/package'
require 'zlib'
require 'zip'
require 'fileutils'

TAR_LONGLINK = '././@LongLink'

class ArchiveUtils
  def self.MAX_FILES
    100
  end
  def self.MAX_SIZE
    10.megabytes
  end

  class FileReadError < Exception
    attr_accessor :file
    attr_accessor :type
    attr_accessor :exn
    def initialize(file, type, exn)
      @file = file
      @type = type
      @exn = exn
    end
    def to_s
      "Could not successfully read #{type} #{file}:\n #{exn}"
    end
  end
  class FileCountLimit < Exception
    attr_accessor :limit
    attr_accessor :file
    def initialize(limit, file)
      @limit = limit
      @file = file
    end
    def to_s
      "Too many entries (more than #{limit}) in #{file}"
    end
  end
  class FileSizeLimit < Exception
    attr_accessor :limit
    attr_accessor :file
    def initialize(limit, file)
      @limit = limit
      @file = file
    end
    def to_s
      "Extracted contents of #{file} are too large (more than #{ActiveSupport::NumberHelper.number_to_human_size(limit)})"
    end
  end
  class SafeExtractionError < Exception
    attr_accessor :file
    attr_accessor :link
    def initialize(file, link)
      @file = file
      if link.is_a? Array
        @link = link
      else
        @link = [link]
      end
    end
    def to_s
      do_does = (@link.length == 1 ? "does" : "do")
      "Could not extract #{file}: #{link.join(', ')} #{do_does} not stay within the extraction directory"
    end
  end

  def self.is_zip?(file, mime)
    mime == "application/zip" || file.ends_with?(".zip")
  end
  def self.is_tar?(file, mime)
    mime == "application/x-tar" || file.ends_with?(".tar")
  end
  def self.is_tar_gz?(file, mime)
    mime == "application/x-compressed-tar" || file.ends_with?(".tar.gz") || file.ends_with?(".tgz")
  end
  def self.is_gz?(file, mime)
    mime == "application/gzip" || file.ends_with?(".gz")
  end
  def self.too_many_files?(file, mime, limit = ArchiveUtils.MAX_FILES)
    if is_zip?(file, mime)
      zip_too_many_files?(file, limit)
    elsif is_tar?(file, mime)
      tar_too_many_files?(file, limit)
    elsif is_tar_gz?(file, mime)
      tar_gz_too_many_files?(file, limit)
    elsif is_gz?(file, mime)
      return false # A .gz file only contains a single file
    else
      return false # it's a single file
    end      
  end

  def self.total_size_too_large?(file, mime, limit = ArchiveUtils.MAX_SIZE)
    if is_zip?(file, mime)
      zip_total_size_too_large?(file, limit)
    elsif is_tar?(file, mime)
      tar_total_size_too_large?(file, limit)
    elsif is_tar_gz?(file, mime)
      tar_gz_total_size_too_large?(file, limit)
    elsif is_gz?(file, mime)
      gzip_total_size_too_large?(file, limit)
    else
      if File.size(file) >= limit
        raise FileSizeLimit.new(file, limit)
      end
      return false
    end
  end

  def self.extract(file, mime, dest)
    # Extracts the file to the given destination
    # Ensures that any symlinks created are entirely local to the destination
    # Assumes that too_many_files? and total_size_too_large? are ok with this file
    # Raises an exception if symlinks are malicious

    if is_zip?(file, mime)
      zip_extract(file, dest)
    elsif is_tar?(file, mime)
      tar_extract(file, dest)
    elsif is_tar_gz?(file, mime)
      tar_gz_extract(file, dest)
    elsif is_gz?(file, mime)
      dest = File.join(dest, File.basename(file, ".gz"))
      gzip_extract(file, dest)
    else
      if File.symlink?(file)
        raise SafeExtractionError.new(file, dest)
      end
      FileUtils.cp(file, dest)
    end
  end


  private

  ##############################
  # File counts
  ##############################
  def self.zip_too_many_files?(file, limit)
    Zip::File.open(file) do |zip| helper_too_many_files?(file, zip, limit) end
  end
  def self.tar_too_many_files?(file, limit)
    File.open(file) do |stream|
      Gem::Package::TarReader.new(stream) do |tar| helper_too_many_files?(file, tar, limit) end
    end
  end
  def self.tar_gz_too_many_files?(file, limit)
    Zlib::GzipReader.open(file) do |stream|
      Gem::Package::TarReader.new(stream) do |tar| helper_too_many_files?(file, tar, limit) end
    end
  end

  def self.helper_too_many_files?(file, stream, limit)
    count = 0
    stream.each do |f|
      count += 1
      if count > limit
        raise FileCountLimit.new(limit, file)
      end
    end
    return false
  rescue FileCountLimit => l
    raise l
  rescue Exception => e
    raise FileReadError.new(file, 'zip', e)
  end

  ##############################
  # File sizes
  ##############################
  def self.zip_total_size_too_large?(file, limit)
    Zip::File.open(file) do |zf|
      total = 0
      zf.each do |f|
        if f.file?
          # Adapted from the implementation of ::Zip::Entry#create_file
          f.get_input_stream do |fs|
            buf = ''
            while (total < limit && !fs.eof?) do
              buf = fs.sysread(limit - total, buf)
              total += buf.length
            end
          end
        end
        if total >= limit
          raise FileSizeLimit.new(limit, file)
        end
      end
    end
    return false
  rescue FileSizeLimit => l
    raise l
  rescue Exception => e
    raise FileReadError.new(file, 'zip', e)
  end

  def self.tar_total_size_too_large?(file, limit)
    if File.size(file) >= limit
      raise FileSizeLimit.new(limit, file)
    end
    return false
  rescue FileSizeLimit => l
    raise l
  rescue Exception => e
    raise FileReadError.new(file, 'tar', e)
  end

  def self.tar_gz_total_size_too_large?(file, limit)
    gzip_total_size_too_large?(file, limit)
  rescue FileSizeLimit => l
    raise l
  rescue FileReadError => r
    raise FileReadError.new(file, 'tgz', r.exn)
  rescue Exception => e
    raise FileReadError.new(file, 'tgz', e)
  end

  def self.gzip_total_size_too_large?(file, limit)
    Zlib::GzipReader.open(file) do |zf|
      while (!zf.eof? && zf.pos < limit) do
        zf.readpartial(limit)
      end
      if zf.pos >= limit
        raise FileSizeLimit.new(limit, file)
      end
    end
    return false
  rescue FileSizeLimit => l
    raise l
  rescue Exception => e
    raise FileReadError.new(file, 'gz', e)
  end

  ##############################
  # File extraction
  ##############################

  public
  def self.safe_realdir(path)
    # Returns the realdirpath of some maximal safe prefix of path by eliminating safe suffixes
    # A safe prefix or suffix is one that doesn't use ./ or ../ anywhere
    # The idea is to generalize realdirpath to accept some/real/path/not/yet/existing,
    # when some/real/path already exists and not/yet/existing is safe.
    # (This could only mean that we'd use mkdir_p to create not/yet/existing
    # within some/real/path, which is indeed nested within some/real/path.)
    
    path = path.squeeze("/") # eliminate consecutive slashes
    path = path.sub(/\/$/, "") # eliminate trailing slash
    until (File.realdirpath(path) rescue false) do
      if path =~ /(^|\/)\.\.$/
        return nil # Doesn't contain any prefix
      else
        path = path.sub(/(^|\/)[^\/]+$/, "")
      end
    end
    return File.realdirpath(path)
  end
  private
  def self.zip_extract(file, dest)
    Zip::File.open(file) do |zf|
      seen_symlinks = false
      zf.each do |entry|
        out = File.join(dest, entry.name)
        if (safe_realdir(out).starts_with?(dest.to_s) rescue false)
          if entry.directory?
            FileUtils.rm_rf out unless File.directory? out
            FileUtils.mkdir_p(File.dirname(out))
          elsif entry.file?
            FileUtils.rm_rf out unless File.file? out
            FileUtils.mkdir_p(File.dirname(out))
            File.open(out, "wb") do |f|
              f.print entry.get_input_stream.read
            end
            FileUtils.chmod entry.unix_perms, out, :verbose => false if entry.unix_perms
          else
            FileUtils.rm_rf out unless File.file? out
            FileUtils.mkdir_p(File.dirname(out))
            seen_symlinks = true
            # skip creating the symlink for now
          end
        else
          puts safe_realdir(out)
          puts dest
          puts file
          puts entry.name
          raise SafeExtractionError.new(file, entry.name)
        end
      end
      if seen_symlinks
        # Now go through again, only for creating the symlinks
        zf.each do |entry|
          if entry.symlink?
            out = File.join(dest, entry.name)
            link_target = entry.get_input_stream.read
            # Using realdirpath because symlinks shouldn't need to create any directories
            if (File.realdirpath(link_target, dest).to_s.starts_with?(dest.to_s) rescue false)
              File.symlink link_target, out
            else
              raise SafeExtractionError.new(file, entry.name)
            end
          end
        end
      end
    end
    return true
  rescue Exception => e
    puts e
    puts e.backtrace
    raise FileReadError.new(file, 'zip', e)
  end

  def self.tar_extract(file, dest)
    File.open(file) do |source| helper_extract(file, source, dest) end
    return true
  end

  def self.tar_gz_extract(file, dest)
    Zlib::GzipReader.open(file) do |source| helper_extract(file, source, dest) end
    return true
  end

  def self.gzip_extract(file, dest)
    Zlib::GzipReader.open(file) do |input_stream|
      File.open(dest, "w") do |output_stream|
        IO.copy_stream(input_stream, output_stream)
      end
    end
    return true
  end

  def self.helper_extract(file, source, destination)
    # from https://dracoater.blogspot.com/2013/10/extracting-files-from-targz-with-ruby.html
    Gem::Package::TarReader.new(source) do |tar|
      seen_symlinks = false
      dest = nil
      tar.each do |entry|
        if entry.full_name == TAR_LONGLINK
          dest = File.join destination, entry.read.strip
          next
        end
        dest ||= (File.join destination, entry.full_name).sub(/\/$/, "")
        if (File.realdirpath(dest).to_s.starts_with?(destination.to_s) rescue false)
          if entry.directory?
            FileUtils.rm_rf dest unless File.directory? dest
            FileUtils.mkdir_p dest, :mode => entry.header.mode, :verbose => false
          elsif entry.file?
            FileUtils.rm_rf dest unless File.file? dest
            FileUtils.mkdir_p(File.dirname(dest))
            File.open dest, "wb" do |f|
              f.print entry.read
            end
            FileUtils.chmod entry.header.mode, dest, :verbose => false if entry.header.mode
          elsif entry.header.typeflag == '2' #Symlink!
            FileUtils.rm_rf dest unless File.file? dest
            FileUtils.mkdir_p(File.dirname(dest))
            seen_symlinks = true
            # skip creating the symlink for now
          end
        else
          raise SafeExtractionError.new(file, entry.full_name)
        end
        dest = nil
      end
      if seen_symlinks
        # Now go through again, only for creating the symlinks
        tar.rewind
        dest = nil
        tar.each do |entry|
          if entry.full_name == TAR_LONGLINK
            dest = File.join destination, entry.read.strip
            next
          end
          dest ||= File.join destination, entry.full_name
          if entry.header.typeflag == '2' #Symlink!
            # Be careful: symlinks should not escape the destination directory
            if File.realdirpath(entry.header.linkname, destination)
                .to_s
                .starts_with?(destination.to_s)
              File.symlink entry.header.linkname, dest
            else
              # where = File.realdirpath(entry.header.linkname, destination)
              raise SafeExtractionError.new(file, entry.full_name)
            end
          end
          dest = nil
        end
      end
    end
  end
end
