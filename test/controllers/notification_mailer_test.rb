require 'test_helper'

class NotificationMailerTest < ActionMailer::TestCase
  setup do
    @prof = create(:user)
    @req  = create(:reg_request)
  end

  test "got_reg_request" do
    mail = NotificationMailer.got_reg_request(@prof, @req,
                                              "http://example.com")
    assert_match "Hi", mail.body.encoded
  end

end
