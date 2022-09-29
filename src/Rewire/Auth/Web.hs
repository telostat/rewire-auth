-- | This module provides API server Web application definition.
module Rewire.Auth.Web where

import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Logger as Wai.Logger
import Rewire.Auth.Web.Api (apiApplication)


-- | Runs the API server Web application.
runWebServer :: Warp.Port -> IO ()
runWebServer port = do
  putStrLn ("API Server is now running on http://localhost:" <> show port)
  Wai.Logger.withStdoutLogger $ \logger -> do
    let settings = (Warp.setPort port . Warp.setLogger logger) Warp.defaultSettings
    Warp.runSettings settings apiApplication
