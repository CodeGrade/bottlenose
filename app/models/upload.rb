require 'audit'
require 'zipruby'
require 'fileutils'
require 'zlib'
require 'find'

class Upload < ApplicationRecord
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
    extract_contents!(metadata[:mimetype] || upload.content_type)
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
    begin
      if meta[:mimetype] == "application/zip" || upload_path.ends_with?(".zip")
        return ZipRuby::Archive.open(upload_path).num_files
      elsif meta[:mimetype] == "application/x-tar" || upload_path.ends_with?(".tar")
        return SubTarball.count_files(upload_path)
      elsif meta[:mimetype] == "application/x-compressed-tar" || upload_path.ends_with?(".tgz")
        return SubTarball.count_files_gz(upload_path)
      elsif meta[:mimetype] == "application/gzip" || upload_path.ends_with?(".gz")
        if submission_path.to_s.ends_with?(".tar.gz")
          return SubTarball.count_files_gz(upload_path)
        else
          return 1
        end
      else
        return 1
      end
    rescue Exception => e
      puts e.message
      raise Exception.new("Could not read archive to count files")
    end
  end

  def extract_contents!(mimetype)
    base = upload_dir
    extracted_path = base.join("extracted")
    return if Dir.exist?(extracted_path)

    extracted_path.mkpath
    if mimetype == "application/zip" ||
       submission_path.to_s.ends_with?(".zip")
      ZipRuby::Archive.open(submission_path.to_s) do |ar|
        ar.each do |zf|
          if zf.directory?
            FileUtils.mkdir_p(extracted_path.join(zf.name))
          else
            dest = extracted_path.join(zf.name)
            dirname = File.dirname(dest)
            FileUtils.mkdir_p(dirname) unless File.exist?(dirname)

            File.open(dest, 'wb') do |f|
              f << zf.read
            end
          end
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
        dest = extracted_path.join(File.basename(file_name, ".gz"))
        Zlib::GzipReader.open(submission_path) do |input_stream|
          File.open(dest, "w") do |output_stream|
            IO.copy_stream(input_stream, output_stream)
          end
        end
      end
    else
      FileUtils.cp(submission_path, extracted_path)
    end
    # TODO (Ben):
    # File.find(extracted_path) do |f|
    #   if File.extname(f) == ".rkt" || File.extname(f) == ".ss"
    #     contents = f.read
    #     process contents with WXME postprocessor
    #   end
    # end
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

    if user_id.nil?
      raise Exception.new("Must set user before storing uploaded file.")
    end

    if Dir.exist?(upload_dir)
      raise Exception.new("Duplicate secret key (1). That's unpossible!")
    end

    Audit.log("User #{user.name} (#{user_id}) creating upload #{secret_key}")

    create_submission_structure(upload, metadata)

    count = count_contents(upload, metadata)
    if count > 100
      raise Exception.new("Too many files (#{count}) in submission!")
    end

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
        if child.file?
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
    p.to_s.gsub(Rails.root.join("public").to_s, "")
  end

  def self.full_path_for(p)
    Rails.root.join("public").to_s + self.upload_path_for(p)
  end

  def cleanup!
    Audit.log("Skip cleanup: #{file_name} for #{user.name} (#{user_id}) at #{secret_key}")
  end

  def self.base_upload_dir
    Rails.root.join("public", "uploads", Rails.env)
  end

  def self.cleanup_test_uploads!
    dir = Rails.root.join("public", "uploads", "test").to_s
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
