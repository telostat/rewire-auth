{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

-- | This module provides authentication API endpoint definitions.
module Rewire.Auth.Web.Api.Endpoints.Auth where

import qualified Rewire.Auth.Web.Api.Endpoints.Auth.Login as Login
import Servant (type (:>))
import qualified Servant


-- | Authentication API definition.
type Api = "auth" :> Login.Api


-- | Authentication API handler.
handler :: Servant.Server Api
handler = Login.handler
