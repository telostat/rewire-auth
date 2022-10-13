{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

-- | This module provides final API definition, handler and application together
-- with the API documentation.
module Rewire.Auth.Web.Api where

import qualified Rewire.Auth.Web.Api.Documentation as Documentation
import qualified Rewire.Auth.Web.Api.Endpoints as Endpoints
import Servant (type (:<|>) ((:<|>)))
import qualified Servant


-- | API definition.
type Api = Documentation.Api :<|> Endpoints.Api


-- | API handler.
handler :: Servant.Server Api
handler = Documentation.handler :<|> Endpoints.handler


-- | API application.
application :: Servant.Application
application = Servant.serve (Servant.Proxy :: Servant.Proxy Api) handler
