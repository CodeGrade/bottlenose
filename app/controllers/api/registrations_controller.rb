module Api
  class RegistrationsController < ApiController
    before_action :find_course
    before_action :require_admin_or_prof

    def index
      registrations_by_section = @course.sections.includes(users: [:registrations]).map do |sec|
        [
          sec.id,
          {
            title: sec.to_s,
            students: sec.students.map do |s|
              serialize_user(s)
            end,
            graders: sec.graders.map do |g|
              serialize_user(g)
            end,
            assistants: sec.assistants.map do |a|
              serialize_user(a)
            end,
            professors: sec.professors.map do |p|
              serialize_user(p)
            end
          }
        ]
      end.to_h
      render json: registrations_by_section
    end
  end
end
