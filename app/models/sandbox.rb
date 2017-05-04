require 'container'

class Sandbox < ActiveRecord::Base
  belongs_to :submission
  before_destroy :stop_container

  def container
    if new_record?
      raise Exception.new("Must save sandbox before getting container")
    end

    if @container.nil?
      @container = Container.new("sandbox-#{id}")
    end
    @container
  end

  def stop_container
    @container.force_stop! if @container
  end
end

