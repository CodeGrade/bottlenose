require 'test_helper'
require 'tempfile'
require 'fileutils'

class FilesControllerTest < ActionController::TestCase

  setup do
    Upload.base_upload_dir.mkpath
  end

  teardown do
    FileUtils.rmdir(Upload.base_upload_dir)
  end

  test "should get valid resource file path" do
    Tempfile.create("foo", Rails.root.join("lib", "assets")) do |f|
      file_name = Pathname.new(f.path).basename
      get "resource", params: {
        path: file_name
      }
      assert_response :success
    end
  end

  test "should error on non existent resource file path" do
    get "resource", params: {
      path: "nonexistent_file.txt"
    }
    assert_response :missing
  end

  test "should error on resource path containing ../" do
    Tempfile.create("foo", Rails.root.join("lib")) do |f|
      file_name = Pathname.new(f.path).basename
      get "resource", params: {
        path: "../#{file_name}"
      }
      assert_response :missing
    end
  end

  test "should get valid upload path" do
    Tempfile.create("foo", Upload.base_upload_dir) do |f|
      file_name = Pathname.new(f.path).basename
      get "upload", params: {
        path: "#{file_name}"
      }
      assert_response :success
    end
  end

  test "should error on non-existent upload path" do
    get "upload", params: {
      path: "non-existent.txt"
    }
    assert_response :missing
  end

  test "should error on upload path containing ../" do
    Tempfile.create("foo", Upload.base_upload_dir.join("../")) do |f|
      file_name = Pathname.new(f.path).basename
      get "upload", params: {
        path: "../#{file_name}"
      }
      assert_response :missing
    end
  end

end