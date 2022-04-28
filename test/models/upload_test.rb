require 'test_helper'

class UploadTest < ActiveSupport::TestCase
  include ActionDispatch::TestProcess
  
  test "Should ensure extracted contents are readable" do
    make_standard_course
    assn = create(:assignment, name: "Assignment 1", course: @cs101, teamset: @ts1,
             available: DateTime.now - 1.weeks, due_date: DateTime.now + 1.weeks,
             lateness_config: @cs101.lateness_config)

    upload_file = fixture_file_upload("Archive-no-perms.zip", "application/octet-stream")
    sub = FilesSub.new(user: @john, assignment: assn, upload_file: upload_file,
                       created_at: assn.due_date - 1.days)
    sub.save_upload
    sub.save
    sub.set_used_everyone!

    upload = sub.upload

    Pathname.new(upload.extracted_path).each_child do |entry|
      stat = File::Stat.new(entry)
      next unless stat.file?
      assert stat.readable?
    end

    Dir.mktmpdir do |d|
      upload.extract_contents_to!(nil, Pathname.new(d))
      Pathname.new(d).each_child do |entry|
        stat = File::Stat.new(entry)
        next unless stat.file?
        assert !stat.readable?
      end
    end
    Dir.mktmpdir do |d|
      upload.extract_contents_to!(nil, Pathname.new(d), force_readable: true)
      Pathname.new(d).each_child do |entry|
        stat = File::Stat.new(entry)
        next unless stat.file?
        assert stat.readable?
      end
    end
  end
end
