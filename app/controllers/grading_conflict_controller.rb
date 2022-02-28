class GradingConflictController < ApplicationController
    
    layout 'course'

    before_action :require_admin_or_staff
    before_action :require_admin_or_prof, only: [:delete, :update]

    def index
    end

    # TODO: Should there be a show method? May not be enough info/data
    # to warrant showing an individual one since it's essentially
    # just a pair of usernames.

    def create
    end

    def delete
    end

    def update
    end

    private

    # TODO: Add param methods
    
end
