{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | This module provides definition for values representing secrets.
module Rewire.Auth.Internal.Secret where

import qualified Data.Aeson as Aeson
import qualified Data.OpenApi as OpenApi
import qualified Data.Text as T


-- | Data definition for textual values which represent secrets.
--
-- Essentially, it is a newtype around 'T.Text' type. The main motivation is to
-- provide custom instances when required.
newtype Secret = Secret
  { unSecret :: T.Text
  }
  deriving (Eq, Aeson.FromJSON, Aeson.ToJSON, OpenApi.ToSchema)


-- | Creates a 'Secret' value from a given
--
-- >>> :set -XOverloadedStrings
-- >>> mkSecret "very-secret"
-- <redacted>
-- >>> unSecret (mkSecret "very-secret")
-- "very-secret"
mkSecret :: T.Text -> Secret
mkSecret = Secret


instance Show Secret where
  show _ = "<redacted>"
