{-# LANGUAGE TypeOperators #-}

-- | This module provides API endpoint definitions.
module Rewire.Auth.Web.Api.Endpoints where

import qualified Rewire.Auth.Web.Api.Endpoints.Auth as Auth
import qualified Rewire.Auth.Web.Api.Endpoints.HealthCheck as HealthCheck
import qualified Rewire.Auth.Web.Api.Endpoints.Version as Version
import Servant (type (:<|>) ((:<|>)))
import qualified Servant


-- | Endpoints API definition.
type Api = Version.Api :<|> HealthCheck.Api :<|> Auth.Api


-- | Endpoints API handler.
handler :: Servant.Server Api
handler = Version.handler :<|> HealthCheck.handler :<|> Auth.handler
