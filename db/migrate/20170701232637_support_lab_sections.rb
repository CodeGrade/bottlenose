class SupportLabSections < ActiveRecord::Migration[5.1]
  def self.up
    add_column :sections, :type, :integer, null: false, default: 0
    add_index :sections, :course_id
    create_table :registration_sections do |t|
      t.integer :registration_id, null: false
      t.integer :section_id, null: false
      t.index :section_id
      t.index :registration_id
    end
    Registration.all.each do |reg|
      RegistrationSection.create!(registration_id: reg.id, section_id: reg.section_id)
    end
    create_table :reg_request_sections do |t|
      t.integer :reg_request_id, null: false
      t.integer :section_id, null: false
      t.index :section_id
      t.index :reg_request_id
    end      
    RegRequest.all.each do |rr|
      RegRequestSection.create!(reg_request_id: rr.id, section_id: rr.section_id)
    end
    remove_column :registrations, :section_id
    remove_column :reg_requests, :section_id
  end
  def self.down
    add_column :reg_requests, :section_id
    add_column :registrations, :section_id
    RegRequestSection.each do |rr|
      rr.reg_request.update_attribute(:section_id, rr.section_id)
    end
    RegistrationSection.each do |reg|
      reg.registration.update_attribute(:section_id, reg.section_id)
    end
    drop_table :reg_request_sections
    drop_table :registration_sections
    change_column_null :reg_requests, :section_id, true
    change_column_null :registrations, :section_id, true
  end
end
