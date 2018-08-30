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
  validates :assignment, :presence => true
  validates :user_id,    :presence => true
  validates :secret_key, :presence => true

  belongs_to :user
  belongs_to :assignment
  has_one :course, through: :assignment

  after_initialize :generate_secret_key!
  before_create :store_upload!
  before_destroy :cleanup!
  after_rollback :purge!

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
    found_any = false
    Find.find(extracted_path) do |f|
      next unless File.file? f
      found_any = true
      next if File.extname(f).empty?
      Postprocessor.process(extracted_path, f)
    end
    Postprocessor.no_files_found(extracted_path) unless found_any
  end
  
  def upload_dir
    pre = secret_key.slice(0, 2)
    Upload.base_upload_dir.join(course&.id.to_i.to_s, assignment&.id.to_i.to_s, pre, secret_key)
  end

  def path
    # Yields a string that's a public uploads path to the submitted file
    Upload.upload_path_for(submission_path)
  end

  def url
    "#{Settings['site_url']}#{path}"
  end

  def upload_data=(upload)
    @upload = upload
  end

  def metadata=(metadata)
    @metadata = metadata
  end
  
  def read_metadata
    begin
      YAML.load(File.open(metadata_path))
    rescue Exception => e
      Audit.log("Problems reading metadata for #{metadata_path}: #{e}")
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
          converted_path = Pathname.new(child.to_s.gsub(extracted_path.to_s,
                                                        extracted_path.dirname.join("converted").to_s))
          converted_path = converted_path.dirname.join(child.basename(child.extname).to_s + ".pdf")
          if File.exists?(converted_path)
            {path: child.basename.to_s,
             full_path: child,
             converted_path: Upload.upload_path_for(converted_path),
             public_link: Upload.upload_path_for(child)}
          else
            {path: child.basename.to_s, full_path: child, public_link: Upload.upload_path_for(child)}
          end
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
      p.gsub(Upload.base_upload_dir.to_s, "/files")
    else
      p
    end
  end

  def self.full_path_for(p)
    p = p.to_s
    if p.starts_with?("/files")
      p.gsub("/files", Upload.base_upload_dir.to_s)
    else
      p
    end
  end

  def cleanup!
    Audit.log("Skip cleanup: #{file_name} for #{user.name} (#{user_id}) at #{secret_key}")
  end

  def purge!
    FileUtils.rm_rf (upload_dir.to_s)
  end

  def self.base_upload_dir
    Rails.root.join("private", "uploads", Rails.env)
  end

  def self.cleanup_test_uploads!
    dir = base_upload_dir.to_s
    if Rails.env === "test" && dir =~ /test/
      FileUtils.rm_rf(dir)
    end
  end

  private
  def store_upload!
    self.file_name = @upload.original_filename

    # Can't set user id in grader upload.
    # Graders don't know *anything*, so it's hard to fake.
    #if user_id.nil?
    #  raise Exception.new("Must set user before storing uploaded file.")
    #end

    if Dir.exist?(upload_dir)
      raise Exception.new("Duplicate secret key (1). That's unpossible!")
    end

    Audit.log("User #{user&.name} (#{user_id}) creating upload #{secret_key}")

    create_submission_structure(@upload, @metadata)

    if @upload.is_a? ActionDispatch::Http::UploadedFile
      upload_path = @upload.path
    elsif @upload.is_a? String
      upload_path = @upload
    else
      upload_path = submission_path.to_s
    end
    effective_mime = @metadata[:mimetype] || @upload.content_type

    unless (metadata.dig(:prof_override, :file_count) rescue false)
      ArchiveUtils.too_many_files?(upload_path, effective_mime, Upload.MAX_FILES) 
    end
    unless (metadata.dig(:prof_override, :file_size) rescue false)
      ArchiveUtils.total_size_too_large?(upload_path, effective_mime, Upload.MAX_SIZE)
    end
    ArchiveUtils.invalid_paths?(upload_path, effective_mime)

    extract_contents!(effective_mime)

    Audit.log("Uploaded file #{file_name} for #{user&.name} (#{user_id}) at #{secret_key}")
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
