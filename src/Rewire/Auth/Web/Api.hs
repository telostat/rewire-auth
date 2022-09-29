{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- | This module provides API endpoint definitions along with the documentation.
module Rewire.Auth.Web.Api where

import Control.Lens ((.~), (?~))
import Control.Lens.Lens ((&))
import qualified Data.OpenApi as OpenApi
import qualified Data.Text as T
import Data.Version (showVersion)
import Paths_rewire_auth (version)
import Rewire.Auth.Web.Api.Endpoints (ApiCore, apiCore, apiCoreServer)
import Servant (type (:<|>) ((:<|>)), type (:>))
import qualified Servant
import qualified Servant.OpenApi
import qualified Servant.Swagger.UI


-- | API type definition for serving both OpenAPI specification file and Swagger UI.
type ApiDocs = "docs" :> Servant.Swagger.UI.SwaggerSchemaUI "swagger-ui" "openapi.json"


-- | OpenAPI specification for the Core API.
openapi :: OpenApi.OpenApi
openapi =
  Servant.OpenApi.toOpenApi apiCore
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


-- | API documentation server implementation.
apiDocsServer :: Servant.Server ApiDocs
apiDocsServer = Servant.Swagger.UI.swaggerSchemaUIServer openapi


-- * Final API


-- | Type definition for final API combining both 'ApiCore' and 'ApiOpenApi'.
type Api = ApiDocs :<|> ApiCore


-- | API definition.
api :: Servant.Proxy Api
api = Servant.Proxy


-- | API server implementation.
apiServer :: Servant.Server Api
apiServer = apiDocsServer :<|> apiCoreServer


-- | API server application.
apiApplication :: Servant.Application
apiApplication = Servant.serve api apiServer
