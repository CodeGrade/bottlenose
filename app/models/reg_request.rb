class RegRequest < ActiveRecord::Base
  enum role: [:student, :grader, :assistant, :professor]

  validates_presence_of :course_id, :user_id

  belongs_to :course
  belongs_to :user

  has_many :reg_request_sections, dependent: :destroy
  has_many :sections, through: :reg_request_sections

  validates :user_id, :uniqueness => { :scope => :course_id }

  Section::types.each do |t, value|
    # This attribute writer is so that the registration form can assign the parameter
    attr_accessor "#{t}_sections".to_sym
  end

  def create_registration
    reg = self.course.registrations.where(user_id: self.user_id)
          .first_or_create(user_id: self.user_id, course_id: self.course_id)
    reg.role = self.role
    reg.show_in_lists = (role == 'student')
    reg.dropped_date = nil # implicitly un-drop user
    reg.new_sections = self.sections
    reg
  end

  before_validation :confirm_sections
  def confirm_sections
    @requested_sections = []
    Section::types.each do |t, value|
      t_sections = self.send("#{t}_sections".to_sym)
      next if t_sections.nil?
      if !t_sections.match?(/^\s*\d+(\s*,\s*\d+)*\s*$/)
        a_an = t.match?(/^[aeiou]/) ? "an" : "a"
        self.errors.add(:base, "Expected to receive #{a_an} #{t} section request")
      else
        @requested_sections += t_sections.split(",").map(&:to_i)
      end
    end
    if @requested_sections.blank?
      self.errors.add(:base, "A new registration must include at least one section")
      return false
    end
    @sections = Section.where(course: self.course, crn: @requested_sections)
    if @sections.length != @requested_sections.count
      self.errors.add(:base, "Could not find all requested sections")
      return false
    elsif @sections.map(&:course_id).uniq.count > 1
      self.errors.add(:base, "Requested sections were not all part of the same course")
      return false
    end
  end
  after_save :create_new_sections

  delegate :name, :email, to: :user

  private
  def create_new_sections
    @sections.each do |s|
      begin
        RegRequestSection.create!(reg_request_id: self.id, section_id: s.id)
      rescue Exception => e
        self.errors.add(:base, "Could not register for section #{s.crn}: #{e}")
        raise e
      end
    end
  end
end
