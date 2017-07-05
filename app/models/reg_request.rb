class RegRequest < ActiveRecord::Base
  enum role: [:student, :grader, :assistant, :professor]

  validates_presence_of :course_id, :user_id

  belongs_to :course
  belongs_to :user

  has_many :reg_request_sections
  has_many :sections, through: :reg_request_sections

  validates :user_id, :uniqueness => { :scope => :course_id }

  Section::types.each do |t, value|
    # This attribute writer is so that the registration form can assign the parameter
    attr_accessor "#{t}_section".to_sym
    # This reader is for convenience
    define_method "#{t}_sections".to_sym do
      self.sections.where(type: value)
    end
  end
  attr_accessor :orig_sections
  attr_accessor :new_sections
  def after_initialize
    @orig_sections ||= []
    @new_sections ||= []
  end
  def save_sections
    remove_old_sections
    create_new_sections
    print self.errors.full_messages.join("\n")
    return self.errors.size != 0
  end

  delegate :name, :email, to: :user

  private
  def remove_old_sections
    @orig_sections&.each do |crn|
      rs = RegRequestSection.find_by(reg_request_id: self.id, section_id: crn)
      if rs.nil?
        self.errors.add(:base, "Could not find section #{crn} to remove enrollment")
      else
        begin
          rs.destroy!
        rescue Exception => e
          self.errors.add(:base, "Could not remove enrollment from section #{crn}: #{e}")
        end
      end
    end
  end
  def create_new_sections
    @new_sections&.each do |crn|
      s = Section.find_by(crn: crn)
      if s.nil?
        self.errors.add(:base, "Could not find section #{crn}")
      else
        begin
          RegRequestSection.create!(reg_request_id: self.id, section_id: crn)
        rescue Exception => e
          self.errors.add(:base, "Could not register for section #{crn}: #{e}")
        end
      end
    end
  end
end
