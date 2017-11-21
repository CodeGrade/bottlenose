class TeamRequest < ApplicationRecord
  belongs_to :teamset
  belongs_to :user

  validate :all_partners_known
  validates :teamset, presence: true
  validates :user, presence: true

  # A TeamRequest is one student's request to work with other students
  # The TeamsetsController will only care about requests that form a clique:
  # Each student mutually has to request to work with the others.

  def teammates
    return @teammates if @teammates
    @teammates = User.where(username: partners)
    return @teammates
  end

  def partners
    return @partners if @partners
    @partners = self.partner_names.split(';')
    @partners.map!(&:strip)
    @partners.sort!
    @partners.uniq!
    self.partner_names = @partners.join(';')
    return @partners
  end

  def all_partners_known
    found = partners.map{|n| [n, false]}.to_h
    teammates.each do |u| found.delete u.username end
    return true if found.empty?
    self.errors.add(:base, "#{pluralize(found.count, 'unknown partner username')} requested: #{found.keys.to_sentence}")
    return false
  end
end
