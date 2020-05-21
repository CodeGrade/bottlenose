module Api
  class CoursesController < ApiController
    before_action -> { find_course(params[:id]) }, only: [:show]
    before_action :require_registered_user, only: [:show]
    before_action :require_admin_or_prof_ever, only: [:index]

    def show
      if current_user_site_admin? || current_user_prof_for?(@course)
        render json: serialize_course(@course)
      else
        render json: @course.slice(:id, :name)
      end
    end

    def index
      render json: serialize_active_courses(current_user.active_courses)
    end

    private

    def serialize_active_courses(active_courses)
      active_courses.map do |sem, courses|
        {
          term: sem,
          courses: courses.map { |c| serialize_course(c) }
        }
      end
    end

    def serialize_course(course)
      {
        id: course.id,
        name: course.name,
        role: current_user.registration_for(course).role
        # students: course.students.map { |s| serialize_student(s) },
        # staff: course.sorted_staff.group_by(&:role).map { |k, v| [Registration::roles.invert[k], v.map { |u| serialize_user(u) }] }.to_h,
        # sections: course.sections.map { |s| serialize_section(s) },
        # registrations: serialize_regs(course.registrations)
      }
    end

    def serialize_student(student)
      student.slice(:username, :display_name)
    end

    def serialize_staff(staff_member, course)
      {
        username: staff_member.username,
        role: staff_member.registration_for(course).role
      }
    end

    def serialize_section(section)
      section.slice(:crn, :meeting_time, :type, :prof_name)
    end

    def serialize_regs(regs)
      mapped = regs.map { |r| serialize_reg(r) }
      mapped.to_h
    end

    def serialize_reg(reg)
      sections = reg.registration_sections.map { |rs| rs.section.id }
      [reg.user.username, sections]
    end
  end
end
