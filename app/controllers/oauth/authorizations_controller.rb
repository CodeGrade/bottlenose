module Oauth
  class AuthorizationsController < Doorkeeper::AuthorizationsController
    impersonates :user
  end
end
