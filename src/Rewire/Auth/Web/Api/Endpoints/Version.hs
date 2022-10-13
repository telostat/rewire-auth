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


-- * API Definition


-- | Version API definition.
type Api =
  "version"
    :> Servant.Summary "Version Endpoint"
    :> Servant.Description "This endpoint returns the version of the application."
    :> Servant.Get '[Servant.JSON] VersionResponsePayload


-- | Version API handler.
handler :: Monad m => m VersionResponsePayload
handler = pure buildVersionResponsePayload


-- * API Implementaion


-- | Data definition of version data.
newtype VersionResponsePayload = VersionResponsePayload
  { unVersionResponsePayload :: T.Text
  }
  deriving (Generic, Aeson.ToJSON, OpenApi.ToSchema)


-- | Builds version response payload representing the current version of the
-- application/library.
buildVersionResponsePayload :: VersionResponsePayload
buildVersionResponsePayload = VersionResponsePayload (T.pack (showVersion version))
