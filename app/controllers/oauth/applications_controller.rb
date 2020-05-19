module Oauth
  class ApplicationsController < Doorkeeper::ApplicationsController
    impersonates :user
  end
end
