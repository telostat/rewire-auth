module Rewire.Auth.Web.Api.Endpoints.Auth where

import Rewire.Auth.Web.Api.Endpoints.Auth.Login (ApiAuthLogin, performAuthLogin)
import qualified Servant


-- | Authentication API type definition.
type ApiAuth = ApiAuthLogin


-- | Authentication API definition.
apiAuth :: Servant.Proxy ApiAuth
apiAuth = Servant.Proxy


-- | Authentication API server implementation.
apiAuthServer :: Servant.Server ApiAuth
apiAuthServer = performAuthLogin
