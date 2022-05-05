require 'test_helper'

class SanityTest < ActionDispatch::IntegrationTest
  test "correct sandbox scripts installed" do
    skip
    sandbox = Rails.root.join("sandbox/scripts")
    install = Pathname.new("/usr/local/bottlenose/scripts")

    %W{build-assignment.sh teardown-directory.sh grading-prep.sh
       setup-directory.sh test-assignment.sh
    }.each do |script|
      assert File.exist?(install.join(script)), "Script installed?"
      ssum = `cat "#{sandbox.join(script)}" | md5sum`
      isum = `cat "#{install.join(script)}" | md5sum`
      assert_equal ssum, isum, "Installed version should match"
    end
  end

  test "factory bot lint" do
    FactoryBot.lint
    assert(true)
  end
end
