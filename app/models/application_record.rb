class ApplicationRecord < ActiveRecord::Base
  self.abstract_class = true
  def pluralize(count, word, plural = nil)
    ActionController::Base.helpers.pluralize(count, word, plural)
  end
end
