{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}

-- | Provides application version data definitions and API endpoint.
module Rewire.Auth.Web.Api.Endpoints.Version where

import qualified Data.Aeson as Aeson
import qualified Data.OpenApi as OpenApi
import qualified Data.Text as T
import Data.Version (showVersion)
import GHC.Generics (Generic)
import Paths_rewire_auth (version)
import Servant (type (:>))
import qualified Servant


-- | Data definition of version data.
newtype VersionData = VersionData
  { unVersionData :: T.Text
  }
  deriving (Generic, Aeson.ToJSON, OpenApi.ToSchema)


-- NOTE: Once we expand the health-check data definition, we will implement the
-- OpenAPI schema as below.
--
-- instance OpenApi.ToSchema VersionData where
--   declareNamedSchema proxy = OpenApi.genericDeclareNamedSchema OpenApi.defaultSchemaOptions proxy
--     & Lens.mapped . OpenApi.schema . OpenApi.description ?~ "Version data definition"
--     & Lens.mapped . OpenApi.schema . OpenApi.example ?~ Aeson.toJSON getCurrentVersion

-- | Returns the 'VersionData' representing the current version of the application/library.
getCurrentVersion :: VersionData
getCurrentVersion = VersionData (T.pack (showVersion version))


-- | Version API type definition.
type ApiVersion =
  "version"
    :> Servant.Summary "Version Endpoint"
    :> Servant.Description "This endpoint returns the version of the application."
    :> Servant.Get '[Servant.JSON] VersionData


-- | Version API definition.
apiVersion :: Servant.Proxy ApiVersion
apiVersion = Servant.Proxy


-- | Version API request handler.
getVersion :: Monad m => m VersionData
getVersion = pure getCurrentVersion
