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


-- | Data definition of health-check data.
--
-- >>> Aeson.encode (HealthCheckData True)
-- "true"
-- >>> Aeson.encode (HealthCheckData False)
-- "false"
newtype HealthCheckData = HealthCheckData
  { healthCheckDataHealthy :: Bool
  }
  deriving (Generic, Aeson.ToJSON, OpenApi.ToSchema, Show)


-- NOTE: Once we expand the health-check data definition, we will implement the
-- OpenAPI schema as below.
--
-- instance OpenApi.ToSchema HealthCheckData where
--   declareNamedSchema proxy = OpenApi.genericDeclareNamedSchema OpenApi.defaultSchemaOptions proxy
--     & Lens.mapped . OpenApi.schema . OpenApi.description ?~ "Health-check data definition"
--     & Lens.mapped . OpenApi.schema . OpenApi.example ?~ Aeson.toJSON (HealthCheckData True)

-- | Health-check API type definition.
type ApiHealthCheck =
  "healthcheck"
    :> Servant.Summary "Health Check Endpoint"
    :> Servant.Description "This endpoint returns information about the health status of the application."
    :> Servant.Get '[Servant.JSON] HealthCheckData


-- | Health-check API definition.
apiHealthCheck :: Servant.Proxy ApiHealthCheck
apiHealthCheck = Servant.Proxy


-- | Health-check API request handler.
getHealthCheck :: Monad m => m HealthCheckData
getHealthCheck = pure (HealthCheckData True)
