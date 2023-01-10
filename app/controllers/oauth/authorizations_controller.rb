module Oauth
  class AuthorizationsController < Doorkeeper::AuthorizationsController
    # Note: Does NOT allow impersonating users so that logging into Hourglass (or other OAuth apps) is disjoint from
    # Bottlenose's impersonation status

    # NOTE: In Rails 7, redirect_to will throw an error if we don't allow_other_host.
    # Unfortunately, Doorkeeper 5.4.0 doesn't supply that option right now, but upgrading
    # to new versions seems to be backwards incompatible for some users(?)
    # For now, monkey-patch this call of redirect_to to add in the necessary flag.
    # Once we successfully upgrade Doorkeeper, we can remove this patch.
    def redirect_to(options = {}, response_options = {})
      response_options[:allow_other_host] = true;
      super(options, response_options)
    end
  end
end
