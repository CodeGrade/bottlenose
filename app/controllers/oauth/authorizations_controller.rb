module Oauth
  class AuthorizationsController < Doorkeeper::AuthorizationsController
    # Note: Does NOT allow impersonating users so that logging into Hourglass (or other OAuth apps) is disjoint from
    # Bottlenose's impersonation status
  end
end
