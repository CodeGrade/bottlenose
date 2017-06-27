require 'audit'
require 'fileutils'
require 'zlib'
require 'find'
require 'open3'

class Upload < ApplicationRecord
  def self.MAX_FILES
    100
  end
  def self.MAX_SIZE
    10.megabytes
  end

  include UploadsHelper
  validates :file_name,  :presence => true
  validates :user_id,    :presence => true
  validates :secret_key, :presence => true

  validate :data_and_metadata_stored

  belongs_to :user

  after_initialize :generate_secret_key!
  before_destroy :cleanup!

  def data_and_metadata_stored
    unless File.exist?(submission_path)
      Audit.log("Uploaded file missing for upload in #{upload_dir}, aborting save.")
      return false
    end

    unless File.exist?(metadata_path)
      Audit.log("Metadata missing for upload in #{upload_dir}, aborting save.")
      return false
    end
    true
  end

  def create_submission_structure(upload, metadata)
    # upload_dir/
    # +-- metadata.yaml
    # +-- submission/
    # |   +-- original_filename
    # +-- extracted/
    # |   ... unzipped contents, or copied file ...
    # +-- graders/
    #     +-- grader_id/
    #     |   ... grader output ...
    #     ... more graders ...
    base = upload_dir
    base.mkpath
    base.join("submission").mkpath
    base.join("graders").mkpath

    store_meta!(metadata)

    self.file_name = upload.original_filename

    File.open(submission_path, 'wb') do |file|
      file.write(upload.read)
    end
  end

  def cleanup_extracted!
    if Dir.exist?(upload_dir.join("extracted"))
      FileUtils.rm_r (upload_dir.join("extracted").to_s)
    end
  end

  def count_contents(upload, meta)
    if upload.is_a? ActionDispatch::Http::UploadedFile
      upload_path = upload.path
    elsif upload.is_a? String
      upload_path = upload
    else
      upload_path = submission_path.to_s
    end
    output, err, status = "", "", ""
    begin
      if meta[:mimetype] == "application/zip" || upload_path.ends_with?(".zip")
        offset = 5 # archive name, column headers, dashes, dashes, and summary lines
        output, status = Open3.capture2("unzip", "-l", upload_path)
      elsif meta[:mimetype] == "application/x-tar" || upload_path.ends_with?(".tar")
        output, err, status = Open3.capture3("tar", "-tvf", upload_path)
        offset = 0
      elsif meta[:mimetype] == "application/x-compressed-tar" || upload_path.ends_with?(".tgz")
        output, err, status = Open3.capture3("tar", "-tzvf", upload_path)
        offset = 0
      elsif meta[:mimetype] == "application/gzip" || upload_path.ends_with?(".gz")
        if submission_path.to_s.ends_with?(".tar.gz")
          output, err, status = Open3.capture3("tar", "-tf", upload_path)
          offset = 0
        else
          return 1
        end
      else
        return 1
      end
      raise Exception.new("Could not list the files in #{upload_path}") if !status.success?
      return output.lines.count - offset
    rescue Exception => e
      puts e.message
      raise Exception.new("Could not read archive to count files")
    end
  end

  def total_size_too_large?(upload, meta)
    if upload.is_a? ActionDispatch::Http::UploadedFile
      upload_path = upload.path
    elsif upload.is_a? String
      upload_path = upload
    else
      upload_path = submission_path.to_s
    end
    begin
      if meta[:mimetype] == "application/zip" || upload_path.ends_with?(".zip")
        output, err, status = Open3.capture3("unzip", "-l", upload_path)
        raise Exception.new("could not list the files in #{upload_path}") if !status.success?
        return output.lines.last.split.first.to_i > Upload.MAX_SIZE
      elsif meta[:mimetype] == "application/x-tar" || upload_path.ends_with?(".tar")
        return File.size(upload_path) > Upload.MAX_SIZE
      elsif meta[:mimetype] == "application/x-compressed-tar" || upload_path.ends_with?(".tgz") ||
            meta[:mimetype] == "application/gzip" || upload_path.ends_with?(".gz")
        # try getting the size info directly from the gzipped file (but size is mod 2^32)
        output, err, status = Open3.capture3("gunzip", "-l", upload_path)
        raise Exception.new("could not list the files in #{upload_path}") if !status.success?
        return true if output.lines.last.split.second.to_i > Upload.MAX_SIZE
        # if the size is "small", it might've actually been greater than 2^32 and overflowed
        # so try the slower, manual approach
        Zlib::GzipReader.open(upload_path) do |zf|
          while (!zf.eof? && zf.pos <= Upload.MAX_SIZE) do
            zf.readpartial(Upload.MAX_SIZE)
          end
          if zf.pos > Upload.MAX_SIZE
            return true
          end
        end
        return false
      else
        return File.size(upload_path) > Upload.MAX_SIZE
      end
    rescue Exception => e
      puts e.message
      raise Exception.new("Could not read archive to measure total file size")
    end
  end

  def extract_contents!(mimetype)
    base = upload_dir
    extracted_path = base.join("extracted")
    return if Dir.exist?(extracted_path)

    extracted_path.mkpath
    if mimetype == "application/zip" ||
       submission_path.to_s.ends_with?(".zip")
      output, err, status = Open3.capture3("unzip", "-n", # Never overwrite
                                           submission_path.to_s, # source
                                           "-d", extracted_path.to_s) # target directory
      if !status.success?
        if output.include?("skipped \"../\"")
          badfiles = output.lines.map do |l|
            match = l.match(/skipped .* path component\(s\) in (.*)/)
            match && match[1]
          end.compact
          if badfiles.length == 1
            raise Exception.new("Could not unzip #{file_name}: #{badfiles} does not stay within the submission directory")
          else
            raise Exception.new("Could not unzip #{file_name}: #{badfiles.join(', ')} do not stay within the submission directory")
          end
        else
          raise Exception.new("Could not unzip #{file_name}: #{status}, #{output}")
        end
      end
    elsif mimetype == "application/x-tar" ||
          submission_path.to_s.ends_with?(".tar")
      SubTarball.untar(submission_path, extracted_path)
    elsif mimetype == "application/x-compressed-tar" ||
          submission_path.to_s.ends_with?(".tgz")
      SubTarball.untar_gz(submission_path, extracted_path)
    elsif mimetype == "application/gzip" ||
          submission_path.to_s.ends_with?(".gz")
      if submission_path.to_s.ends_with?(".tar.gz")
        SubTarball.untar_gz(submission_path, extracted_path)
      else
        FileUtils.copy(submission_path, extracted_path)
        output, err, status = Open3.capture3("gunzip", extracted_path.join(File.basename(file_name, ".gz")).to_s)
        return false if !status.success?
      end
    else
      FileUtils.cp(submission_path, extracted_path)
    end
    Find.find(extracted_path) do |f|
      next unless File.file? f
      next if File.extname(f).empty?
      Postprocessor.process(f)
    end
  end

  def upload_dir
    pre = secret_key.slice(0, 2)
    Upload.base_upload_dir.join(pre, secret_key)
  end

  def path
    # Yields a string that's a public uploads path to the submitted file
    Upload.upload_path_for(submission_path)
  end

  def store_upload!(upload, metadata)
    self.file_name = upload.original_filename

    # Can't set user id in grader upload.
    # Graders don't know *anything*, so it's hard to fake.
    #if user_id.nil?
    #  raise Exception.new("Must set user before storing uploaded file.")
    #end

    if Dir.exist?(upload_dir)
      raise Exception.new("Duplicate secret key (1). That's unpossible!")
    end

    Audit.log("User #{user.name} (#{user_id}) creating upload #{secret_key}")

    create_submission_structure(upload, metadata)

    count = count_contents(upload, metadata)
    if count > Upload.MAX_FILES
      raise Exception.new("Too many files (more than #{Upload.MAX_FILES}) in submission!")
    end
    if total_size_too_large?(upload, metadata)
      raise Exception.new("Extracted files are too large (more than #{ActiveSupport::NumberHelper.number_to_human_size(Upload.MAX_SIZE)})")
    end

    extract_contents!(metadata[:mimetype] || upload.content_type)

    Audit.log("Uploaded file #{file_name} for #{user.name} (#{user_id}) at #{secret_key}")
  end

  def read_metadata
    begin
      YAML.load(File.open(metadata_path))
    rescue Exception => e
      Audit.log("Problems reading metadata for #{metadata_path}: #{e}\n")
      {}
    end
  end

  def extracted_files
    def rec_path(path)
      path.children.sort.collect do |child|
        if child.symlink?
          {
            path: child.basename.to_s,
            link_to: Upload.upload_path_for(child.dirname.join(File.readlink(child))),
            broken: (!File.exists?(File.realpath(child)) rescue true)
          }
        elsif child.file?
          {path: child.basename.to_s, full_path: child, public_link: Upload.upload_path_for(child)}
        elsif child.directory?
          {path: child.basename.to_s, children: rec_path(child)}
        end
      end
    end
    rec_path(extracted_path)
  end


  def extracted_path
    upload_dir.join("extracted")
  end

  def submission_path
    upload_dir.join("submission", file_name)
  end

  def metadata_path
    upload_dir.join("metadata.yaml")
  end

  def grader_path(grader)
    upload_dir.join("graders", grader.id.to_s)
  end

  def self.upload_path_for(p)
    p.to_s.gsub(Rails.root.join("private", "uploads", Rails.env).to_s, "/files")
  end

  def self.full_path_for(p)
    Rails.root.join("private").to_s + self.upload_path_for(p)
  end

  def cleanup!
    Audit.log("Skip cleanup: #{file_name} for #{user.name} (#{user_id}) at #{secret_key}")
  end

  def self.base_upload_dir
    Rails.root.join("private", "uploads", Rails.env)
  end

  def self.cleanup_test_uploads!
    dir = base_upload_dir.join("test").to_s
    if dir.length > 8 && dir =~ /test/
      FileUtils.rm_rf(dir)
    end
  end

  def store_meta!(meta)
    if File.exist?(metadata_path)
      raise Exception.new("Attempt to reset metadata on upload.")
    end

    File.open(metadata_path, "w") do |file|
      file.write(meta.to_yaml)
    end
  end

  def generate_secret_key!
    return unless new_record?

    unless secret_key.nil?
      raise Exception.new("Can't generate a second secret key for an upload.")
    end

    self.secret_key = SecureRandom.urlsafe_base64

    if Dir.exist?(upload_dir)
      raise Exception.new("Duplicate secret key (2). That's unpossible!")
    end
  end
end
