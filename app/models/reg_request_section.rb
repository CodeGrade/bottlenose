class RegRequestSection < ApplicationRecord
  belongs_to :registration
  belongs_to :section, :foreign_key => "section_id", :primary_key => "crn"
end
