{-# LANGUAGE OverloadedStrings #-}

module Rewire.Auth.Internal.Password where

import qualified Data.Text as T


-- | Data definition for password values in a valid format.
newtype Password = MkPassword
  { unPassword :: T.Text
  }
  deriving (Eq, Show)


-- | Attempts to create a 'Password' value with the given text.
--
-- >>> mkPassword ""
-- Left "Password can not be less than 8 characters"
-- >>> mkPassword "1234567"
-- Left "Password can not be less than 8 characters"
-- >>> mkPassword "12345678"
-- Right (MkPassword {unPassword = "12345678"})
mkPassword :: T.Text -> Either T.Text Password
mkPassword x
  | T.length x < 8 = Left "Password can not be less than 8 characters"
  | otherwise = Right (MkPassword x)
