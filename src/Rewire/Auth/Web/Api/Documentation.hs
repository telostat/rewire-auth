{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- | This module provides API documentation definitions.
module Rewire.Auth.Web.Api.Documentation where

import Control.Lens ((&), (.~), (?~))
import qualified Data.OpenApi as OpenApi
import qualified Data.Text as T
import Data.Version (showVersion)
import Paths_rewire_auth (version)
import qualified Rewire.Auth.Web.Api.Endpoints as Endpoints
import Servant (type (:>))
import qualified Servant
import qualified Servant.OpenApi
import qualified Servant.Swagger.UI


-- * API Definition


-- | Documentation API definition.
--
-- Note that this API definition is for serving both OpenAPI specification file
-- and Swagger UI.
type Api = "docs" :> Servant.Swagger.UI.SwaggerSchemaUI "swagger-ui" "openapi.json"


-- | API documentation server implementation.
handler :: Servant.Server Api
handler = Servant.Swagger.UI.swaggerSchemaUIServer openapi


-- * API Implementation


-- | OpenAPI specification for the Core API.
openapi :: OpenApi.OpenApi
openapi =
  Servant.OpenApi.toOpenApi (Servant.Proxy :: Servant.Proxy Endpoints.Api)
    & OpenApi.info . OpenApi.title .~ "Rewire Auth API Documentation"
    & OpenApi.info . OpenApi.description ?~ "This API provides Rewire Authentication functionality."
    & OpenApi.info . OpenApi.termsOfService ?~ url
    & OpenApi.info . OpenApi.contact ?~ contact
    & OpenApi.info . OpenApi.license ?~ ("This work is licensed under MIT license." & OpenApi.url ?~ OpenApi.URL "https://spdx.org/licenses/MIT.html")
    & OpenApi.info . OpenApi.version .~ T.pack (showVersion version)
  where
    email = "info@telostat.com"
    url = "https://telostat.com"
    contact = OpenApi.Contact (Just "Telostat Pte Ltd") (Just (OpenApi.URL url)) (Just email)
