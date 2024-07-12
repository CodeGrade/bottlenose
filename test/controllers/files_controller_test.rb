require 'test_helper'
require 'tempfile'
require 'fileutils'

class FilesControllerTest < ActionController::TestCase

  @@method_to_dir = {
    "upload": Upload.base_upload_dir,
    "resource": Rails.root.join("lib/assets")
  }

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

end
