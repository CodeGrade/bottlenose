class NotificationMailer < ActionMailer::Base
  default from: Settings['site_email']

  # Subject can be set in your I18n file at config/locales/en.yml
  # with the following lookup:
  #
  #   en.notification_mailer.got_reg_request.subject
  #
  def got_reg_request(teacher, req, base_url)
    @teacher  = teacher
    @req      = req
    @course   = req.course
    @base_url = base_url

    mail(to: @teacher.email, subject: "Got reg request")
  end
end
