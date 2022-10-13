{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}

-- | Provides application health-check data definitions and API endpoint.
module Rewire.Auth.Web.Api.Endpoints.HealthCheck where

import qualified Data.Aeson as Aeson
import qualified Data.OpenApi as OpenApi
import GHC.Generics (Generic)
import Servant (type (:>))
import qualified Servant


-- * API Definition


-- | Health-check API definition.
type Api =
  "healthcheck"
    :> Servant.Summary "Health Check Endpoint"
    :> Servant.Description "This endpoint returns information about the health status of the application."
    :> Servant.Get '[Servant.JSON] HealthCheckResponsePayload


-- | Health-check API handler.
handler :: Monad m => m HealthCheckResponsePayload
handler = pure (HealthCheckResponsePayload True)


-- * API Implementation


-- | Data definition of health-check data.
--
-- >>> Aeson.encode (HealthCheckResponsePayload True)
-- "true"
-- >>> Aeson.encode (HealthCheckResponsePayload False)
-- "false"
newtype HealthCheckResponsePayload = HealthCheckResponsePayload
  { unHealthCheckResponsePayload :: Bool
  }
  deriving (Generic, Aeson.ToJSON, OpenApi.ToSchema, Show)
