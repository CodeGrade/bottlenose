require 'securerandom'
require 'audit'

class FilesSub < Submission
  validate :ensure_fields_valid
  def ensure_fields_valid
    if self.assignment.request_time_taken && self.time_taken.to_s.empty?
      self.errors.add(:base, "Please specify how long you have worked on this assignment")
    elsif self.time_taken && !(Float(self.time_taken) rescue false)
      self.errors.add(:base, "Please specify a valid number for how long you have worked on this assignment")
    end
    if self.upload.nil? || self.upload.new_record?
      if @upload_data.nil?
        self.errors.add(:base, "You need to submit a file.")
      end
    end
    return self.errors.count == 0
  end
end
