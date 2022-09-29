{-# LANGUAGE TypeOperators #-}

-- | This module provides core API endpoint definitions.
module Rewire.Auth.Web.Api.Endpoints where

import Rewire.Auth.Web.Api.Endpoints.HealthCheck (ApiHealthCheck, getHealthCheck)
import Rewire.Auth.Web.Api.Endpoints.Version (ApiVersion, getVersion)
import Servant (type (:<|>) ((:<|>)))
import qualified Servant


-- | Core API type definition.
type ApiCore = ApiVersion :<|> ApiHealthCheck


-- | Core API definition.
apiCore :: Servant.Proxy ApiCore
apiCore = Servant.Proxy


-- | Core API server implementation.
apiCoreServer :: Servant.Server ApiCore
apiCoreServer = getVersion :<|> getHealthCheck
