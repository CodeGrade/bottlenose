class AddLatenessCourseId < ActiveRecord::Migration[5.1]
  def up
    add_column :lateness_configs, :course_id, :integer

    course = Course.all.first
    LatenessConfig.all.each do |late|
      late.course_id = course.id
      late.save!
    end

    Course.all.each do |course|
      late = course.lateness_config
      late.course_id = course.id
      late.save!

      course.assignments.each do |asgn|
        late = asgn.lateness_config
        late.course_id = course.id
        late.save!
      end
    end

    change_column_null :lateness_configs, :course_id, false
    change_column_null :courses, :lateness_config_id, true
  end

  def down
    remove_column :lateness_configs, :course_id
  end
end

