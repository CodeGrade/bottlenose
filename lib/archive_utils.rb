require 'audit'
require 'rubygems/package'
require 'zlib'
require 'zip'
require 'fileutils'
require 'stringio'

TAR_LONGLINK = '././@LongLink'

# Since Zip::File and Gem::Package::TarReader are autoloaded,
# simply reopening the classes will fail.  These two patches
# harmonize the interfaces to both Zips and Tars, so that we
# can extract them using the same logic.
Zip::File.class_exec do
  class WrapZipEntry
    def initialize(e)
      @entry = e
    end
    def read
      @entry.get_input_stream.read
    end
    def directory?
      @entry.directory?
    end
    def file?
      @entry.file?
    end
    def symlink?
      @entry.symlink?
    end
    def name
      @entry.name
    end
    def unix_perms
      if @entry.directory?
        nil
      else
        @entry.unix_perms
      end
    end
  end
  def safe_each
    self.each do |e|
      yield(WrapZipEntry.new(e))
    end
  end
end
Gem::Package::TarReader.class_exec do
  class WrapTarEntry
    def initialize(e, name)
      @entry = e
      @name = name
    end
    def read
      @entry.read
    end
    def directory?
      @entry.directory?
    end
    def file?
      @entry.file?
    end
    def symlink?
      @entry.symlink?
    end
    def name
      @name
    end
    def unix_perms
      @entry.header.mode
    end
  end
  def safe_each
    # from https://dracoater.blogspot.com/2013/10/extracting-files-from-targz-with-ruby.html
    self.rewind
    dest = ""
    self.each do |entry|
      if entry.full_name == TAR_LONGLINK
        dest = dest + entry.read.strip
        next
      else
        yield(WrapTarEntry.new(entry, dest + entry.full_name))
        dest = ""
      end
    end
  end
end
class ArchiveUtils
  def self.ARCHIVE_EXTENSIONS
    # Supported file types
    known = {
      tar: true,
      gz: true,
      tgz: true,
      zip: true
    }
    # Unsupported file types, taken from https://en.wikipedia.org/wiki/List_of_archive_formats
    [ "rar", "ar", "cpio", "shar", "lbr", "iso", "mar", "sbx", "bz2", "lz", "lzma", "lzo", "sfark", "sz",
      "xz", "z", "7z", "s7z", "ace", "afa", "alz", "apk", "arc", "arj", "b1", "b6z", "ba", "bh", "cab",
      "car", "cfs", "cpt", "dar", "dd", "dgc", "dmg", "ear", "gca", "ha", "hki", "ice", "jar", "kgb",
      "lzh", "lha", "pak", "partimg", "paq6", "paq7", "paq8", "pea", "pim", "pit", "qda", "rk", "sda",
      "sea", "sen", "sfx", "shk", "sit", "sitx", "sqx", "uc", "uc0", "uc2", "ucn", "ur2", "ue2", "uca",
      "uha", "war", "wim", "xar", "xp3", "yz1", "zipx", "zoo", "zpaq", "zz", "ecc", "par", "par2", "rev"
    ].each do |ext|
      known[ext] = false
    end
    known
  end
  def self.MAX_FILES
    125
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
    attr_accessor :dest
    def initialize(file, dest, link)
      @file = file
      @dest = dest
      if link.is_a? Array
        @link = link
      else
        @link = [link]
      end
    end
    def to_s
      do_does = (@link.length == 1 ? "does" : "do")
      "Could not extract #{file} to #{dest}: #{link.join(', ')} #{do_does} not stay within the extraction directory"
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

  def self.invalid_paths?(file, mime)
    if is_zip?(file, mime)
      zip_invalid_paths?(file)
    elsif is_tar?(file, mime)
      tar_invalid_paths?(file)
    elsif is_tar_gz?(file, mime)
      tar_gz_invalid_paths?(file)
    elsif is_gz?(file, mime)
      if !(file.encode("utf-8").valid_encoding? rescue false)
        raise FileReadError(file, mime, "File name is not valid UTF-8")
      end
      return false # A .gz file only contains a single file
    else
      if !(file.encode("utf-8").valid_encoding? rescue false)
        raise FileReadError(file, mime, "File name is not valid UTF-8")
      end
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

  def self.extract(file, mime, dest, force_readable: false)
    # Extracts the file to the given destination
    # Ensures that any symlinks created are entirely local to the destination
    # Assumes that too_many_files? and total_size_too_large? are ok with this file
    # Raises an exception if symlinks are malicious


    if is_zip?(file, mime)
      zip_extract(file, dest, force_readable)
    elsif is_tar?(file, mime)
      tar_extract(file, dest, force_readable)
    elsif is_tar_gz?(file, mime)
      tar_gz_extract(file, dest, force_readable)
    elsif is_gz?(file, mime)
      dest = File.join(dest, File.basename(file, ".gz"))
      gzip_extract(file, dest, force_readable)
    else
      if File.symlink?(file)
        raise SafeExtractionError.new(file, dest, nil)
      end
      FileUtils.cp(file, dest)
      FileUtils.chmod "u+r", dest, verbose: false if force_readable
    end
  end

  def self.entries(file, mime, from_stream: nil)
    if is_zip?(file, mime)
      zip_entries(file, from_stream)
    elsif is_tar?(file, mime)
      tar_entries(file, from_stream)
    elsif is_tar_gz?(file, mime)
      tar_gz_entries(file, from_stream)
    elsif is_gz?(file, mime)
      [[File.basename(file, ".gz"), true]].to_h
    else
      [[file, true]].to_h
    end
  end
  
  private

  ##############################
  # File counts
  ##############################
  def self.zip_too_many_files?(file, limit)
    Zip::File.open(file) do |zip| helper_too_many_files?(file, 'zip', zip, limit) end
  end
  def self.tar_too_many_files?(file, limit)
    File.open(file) do |stream|
      Gem::Package::TarReader.new(stream) do |tar| helper_too_many_files?(file, 'tar', tar, limit) end
    end
  end
  def self.tar_gz_too_many_files?(file, limit)
    Zlib::GzipReader.open(file) do |stream|
      Gem::Package::TarReader.new(stream) do |tar| helper_too_many_files?(file, 'tgz', tar, limit) end
    end
  end

  def self.helper_too_many_files?(file, type, stream, limit)
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
    raise FileReadError.new(file, type, e)
  end


  ##############################
  # Valid path names
  ##############################
  def self.zip_invalid_paths?(file)
    Zip::File.open(file) do |zip| return helper_invalid_paths?(file, 'zip', zip) end
  end
  def self.tar_invalid_paths?(file)
    File.open(file) do |stream|
      Gem::Package::TarReader.new(stream) do |tar| return helper_invalid_paths?(file, 'tar', tar) end
    end
  end
  def self.tar_gz_invalid_paths?(file)
    Zlib::GzipReader.open(file) do |stream|
      Gem::Package::TarReader.new(stream) do |tar| return helper_invalid_paths?(file, 'tgz', tar) end
    end
  end

  def self.helper_invalid_paths?(file, type, stream)
    stream.safe_each do |entry|
      if !(entry.name.encode("utf-8").valid_encoding? rescue false)
        raise FileReadError.new(file, type, "Entry name `#{entry.name}` is not valid UTF-8")
      end
    end
    return false
  rescue FileReadError => e
    raise e
  rescue Exception => e
    raise FileReadError.new(file, type, e)
  end
  
  ##############################
  # Entries
  ##############################
  def self.zip_entries(file, stream)
    if stream
      Zip::File.open_buffer(stream) do |zip| return helper_entries(file, 'zip', zip) end
    else
      Zip::File.open(file) do |zip| return helper_entries(file, 'zip', zip) end
    end
  end
  def self.tar_entries(file, stream)
    stream =
      if stream then
        StringIO.new(stream)
      else
        File.open(file)
      end
    Gem::Package::TarReader.new(stream) do |tar| return helper_entries(file, 'tar', tar) end
  end
  def self.tar_gz_entries(file, stream)
    stream =
      if stream then
        Zlib::GzipReader.new(StringIO.new(stream))
      else
        Zlib::GzipReader.open(file)
      end
    Gem::Package::TarReader.new(stream) do |tar| return helper_entries(file, 'tgz', tar) end
  end

  def self.helper_entries(file, type, stream)
    output = {}
    stream.safe_each do |entry|
      out = encode_or_escape(File.join("/", entry.name.gsub("\\", "/").sub(/\/$/, "")))
      if out.to_s.match?("__MACOSX") || out.to_s.match?(".DS_Store")
        next
      end
      out = out.squeeze("/") # eliminate consecutive slashes
      out = out.sub(/\/$/, "") # eliminate trailing slash
      if (out.starts_with?(File::SEPARATOR) rescue false)
        temp = output
        File.dirname(out).to_s.split(File::SEPARATOR).each do |dir|
          next if dir.blank?
          temp[dir] = {} if temp[dir].nil?
          temp = temp[dir]
        end
        temp[File.basename(out)] = true unless entry.directory?
      else
        puts "Problem with #{entry.name}"
        #raise SafeExtractionError.new(file, "./", entry.name)
      end
    end
    return output
  rescue FileReadError => e
    raise e
  rescue Exception => e
    raise FileReadError.new(file, type, e)
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

  private
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
        old_path = path
        path = path.sub(/(^|\/)[^\/]+$/, "")
      end
    end
    return File.realdirpath(path)
  end
  def self.encode_or_escape(str)
    begin
      str.encode("utf-8")
    rescue Exception => e
      str.force_encoding("utf-8")
      if str.valid_encoding?
        str
      else
        str.scrub{|bytes| '<'+bytes.unpack('H*')[0]+'>' }
      end
    end
  end
  def self.helper_extract(file, type, archive, dest, force_readable)
    seen_symlinks = false
    archive.safe_each do |entry|
      out = encode_or_escape(File.join(dest, entry.name.gsub("\\", "/").sub(/\/$/, "")))
      if out.to_s.match?("__MACOSX") || out.to_s.match?(".DS_Store")
        next
      end
      if (safe_realdir(out).starts_with?(dest.to_s) rescue false)
        if entry.directory?
          FileUtils.rm_rf out unless File.directory? out
          FileUtils.mkdir_p out, mode: entry.unix_perms, verbose: false
        elsif entry.file?
          FileUtils.rm_rf out unless File.file? out
          FileUtils.mkdir_p(File.dirname(out))
          File.open(out, "wb") do |f|
            f.print entry.read
          end
          FileUtils.chmod entry.unix_perms, out, verbose: false if entry.unix_perms
          FileUtils.chmod "u+r", out, verbose: false if force_readable
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
        raise SafeExtractionError.new(file, dest, entry.name)
      end
    end
    if seen_symlinks
      # Now go through again, only for creating the symlinks
      archive.safe_each do |entry|
        if entry.symlink?
          out = encode_or_escape(File.join(dest, entry.name))
          link_target = entry.read
          # Using realdirpath because symlinks shouldn't need to create any directories
          if (File.realdirpath(link_target, dest).to_s.starts_with?(dest.to_s) rescue false)
            File.symlink link_target, out
          else
            raise SafeExtractionError.new(file, dest.to_s, entry.name)
          end
        end
      end
    end
    return true
  rescue Exception => e
    puts e
    puts e.backtrace
    raise FileReadError.new(file, type, e)
  end

  def self.zip_extract(file, dest, force_readable)
    Zip::File.open(file) do |zf| helper_extract(file, 'zip', zf, dest, force_readable) end
  end
  def self.tar_extract(file, dest, force_readable)
    File.open(file) do |source| helper_extract(file, 'tar', Gem::Package::TarReader.new(source), dest, force_readable) end
  end
  def self.tar_gz_extract(file, dest, force_readable)
    Zlib::GzipReader.open(file) do |source| helper_extract(file, 'tar_gz', Gem::Package::TarReader.new(source), dest, force_readable) end
  end
  def self.gzip_extract(file, dest, force_readable)
    Zlib::GzipReader.open(file) do |input_stream|
      File.open(dest, "w") do |output_stream|
        IO.copy_stream(input_stream, output_stream)
      end
      FileUtils.chmod "u+r", dest, verbose: false if force_readable
    end
    return true
  end
end
