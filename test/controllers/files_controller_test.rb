require 'test_helper'
require 'tempfile'
require 'fileutils'

class FilesControllerTest < ActionController::TestCase

  @@method_to_dir = {
    "upload": Upload.base_upload_dir,
    "resource": Rails.root.join("lib/assets")
  }

  setup do
    unless File.directory? Upload.base_upload_dir
      FileUtils.mkdir Upload.base_upload_dir
    end
  end

  test "should get valid file path" do
    @@method_to_dir.each do |method, dir|
      Tempfile.create("foo", dir) do |f|
        file_name = Pathname.new(f.path).basename
        get method, params: {
          path: file_name
        }
        assert_response :success
      end
    end
  end

  test "should error on non existent file path" do
    @@method_to_dir.each do |method, _|
      get method, params: {
        path: "nonexistent_file.txt"
      }
      assert_response :missing
    end
  end

  test "should error on path containing ../" do
    @@method_to_dir.each do |method, dir|
      Tempfile.create("foo", dir.join("..")) do |f|
        file_name = Pathname.new(f.path).basename
        get method, params: {
          path: "../#{file_name}"
        }
        assert_response :missing
      end
    end
  end

  test "should error on upload path with grader secret" do
    top_level_folder = 'submission'
    grader_path_prefix = 'graders/0'
    dir = Pathname.new(File.join(@@method_to_dir[:upload], top_level_folder, grader_path_prefix))
    begin
      dir.mkpath
      Tempfile.create(['foo', '.secret'], dir) do |f|
        file_name = Pathname.new(f.path).basename
        get "upload", params: {
          path: File.join(top_level_folder, grader_path_prefix, file_name).to_s
        }
        assert_response :missing
      end
    ensure
      FileUtils.rm_rf(File.join(@@method_to_dir[:upload], top_level_folder))
    end
  end
end
