class SubmissionEnabledToggle < ApplicationRecord
  belongs_to :assignment
  belongs_to :section
end
