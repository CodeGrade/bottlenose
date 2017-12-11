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
    @teammates = User.where(username: partners).to_a
    return @teammates
  end

  def to_s
    teammates.reject{|t| t.username == self.user.username}.map(&:display_name).sort.to_sentence
  end

  def request_status
    requests = TeamRequest.where(user_id: teammates.map(&:id)).to_a
    if requests.length != teammates.count
      return "Waiting for other partners to confirm request"
    end
    requests << self
    ids_from_usernames = teammates.map{|t| [t.username, t.id]}.to_h
    ids_from_usernames[self.user.username] = self.user.id
    teammate_requests = teammates.map{|t| [t.id, 0]}.to_h
    teammate_requests[self.user_id] = 0
    requests.each do |r|
      teammate_requests[r.user_id] += 1
      r.partners.each do |p|
        if teammate_requests[ids_from_usernames[p]]
          teammate_requests[ids_from_usernames[p]] += 1
        else
          return "At least one requested teammate has requested a partner you did not."
        end
      end
    end
    if teammate_requests.all?{|partner, count| count == requests.length}
      return "Ready"
    else
      return "At least one requested teammate has not requested a partner that you have requested."
    end
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
