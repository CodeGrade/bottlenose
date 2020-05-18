class DoorkeeperApplicationsController < Doorkeeper::ApplicationsController
  impersonates :user
end
