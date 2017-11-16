require 'audit'
require 'fileutils'
require 'zlib'
require 'find'
require 'open3'
require 'archive_utils'

class Upload < ApplicationRecord
  def self.MAX_FILES
    ArchiveUtils.MAX_FILES
  end
  def self.MAX_SIZE
    ArchiveUtils.MAX_SIZE
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

  def extract_contents!(mimetype)
    return if Dir.exist?(extracted_path)

    extract_contents_to!(mimetype, extracted_path, true)
  end

  def extract_contents_to!(mimetype, extracted_path, postprocess)
    extracted_path.mkpath
    ArchiveUtils.extract(submission_path.to_s, mimetype, extracted_path.to_s)
    return unless postprocess
    Find.find(extracted_path) do |f|
      next unless File.file? f
      next if File.extname(f).empty?
      Postprocessor.process(extracted_path, f)
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

  def url
    "#{Settings['site_url']}#{path}"
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

    Audit.log("User #{user&.name} (#{user_id}) creating upload #{secret_key}")

    create_submission_structure(upload, metadata)

    if upload.is_a? ActionDispatch::Http::UploadedFile
      upload_path = upload.path
    elsif upload.is_a? String
      upload_path = upload
    else
      upload_path = submission_path.to_s
    end
    effective_mime = metadata[:mimetype] || upload.content_type

    ArchiveUtils.too_many_files?(upload_path, effective_mime, Upload.MAX_FILES)
    ArchiveUtils.total_size_too_large?(upload_path, effective_mime, Upload.MAX_SIZE)

    extract_contents!(effective_mime)

    Audit.log("Uploaded file #{file_name} for #{user&.name} (#{user_id}) at #{secret_key}")
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
    p = p.to_s
    if p.starts_with?(Rails.root.to_s)
      p.gsub(Rails.root.join("private", "uploads", Rails.env).to_s, "/files")
    else
      p
    end
  end

  def self.full_path_for(p)
    p = p.to_s
    if p.starts_with?("/files")
      p.gsub("/files", Rails.root.join("private", "uploads", Rails.env).to_s)
    else
      p
    end
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
