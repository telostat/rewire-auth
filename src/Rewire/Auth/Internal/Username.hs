{-# LANGUAGE OverloadedStrings #-}

module Rewire.Auth.Internal.Username where

import qualified Data.Text as T


-- | Data definition for username values in a valid format.
newtype Username = MkUsername
  { unUsername :: T.Text
  }
  deriving (Eq, Show)


-- | Attempts to create a 'Username' value with the given text.
--
-- >>> mkUsername ""
-- Left "Username can not be less than 8 characters"
--
-- TODO: Allow only a syntax for something like @[a-zA-Z][a-zA-Z0-9\-\.]{2,n}@
mkUsername :: T.Text -> Either T.Text Username
mkUsername x
  | T.length x < 3 = Left "Username can not be less than 8 characters"
  | otherwise = Right (MkUsername x)
