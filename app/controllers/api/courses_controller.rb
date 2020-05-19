module Api
  class CoursesController < ApiController
    before_action -> { find_course(params[:id]) }, only: [:show]
    before_action :require_admin_or_prof, only: [:show]
    before_action :require_admin_or_prof_ever, only: [:index]

    def show
      # {
      #   sections: [],
      #   staff: [{ username, role }],
      #   students: [user],
      #   registrations: {
      #     [username]: [sectionId]
      #   },
      # }
      render json: serialize_course(@course)
    end

    def index
      # [
      #   {
      #     term: { ... },
      #     courses: [ { ... } ],
      #   }
      # ] sorted chronologically
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
        students: course.students.map { |s| serialize_student(s) },
        staff: course.staff.map { |s| serialize_staff(s, course) }
      }
    end

    def serialize_student(student)
      student.username
    end

    def serialize_staff(staff_member, course)
      {
        username: staff_member.username,
        role: staff_member.registration_for(course).role
      }
    end
  end
end
