class MakeCrnsNonunique < ActiveRecord::Migration[5.1]
  def up
    remove_index :sections, [:crn]
    add_index :sections, [:crn, :course_id], unique: true
    # this migration is safe only because all CRNS > 10000, and all ids < 100 (so far)
    Section.all.each do |section|
      RegistrationSection.where(section_id: section.crn).update_all(section_id: section.id)
      RegRequestSection.where(section_id: section.crn).update_all(section_id: section.id)
    end
  end
  def down
    remove_index :sections, [:crn, :course_id]
    add_index :sections, [:crn], unique: true
    Section.all.each do |section|
      RegistrationSection.where(section_id: section.id).update_all(section_id: section.crn)
      RegRequestSection.where(section_id: section.id).update_all(section_id: section.crn)
    end
  end
end
