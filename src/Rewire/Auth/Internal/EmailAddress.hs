{-# LANGUAGE OverloadedStrings #-}

module Rewire.Auth.Internal.EmailAddress where

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Text.Email.Validate as Email


-- | Data definition for valid email addresses.
newtype EmailAddress = MkEmailAddress
  { unEmailAddress :: T.Text
  }
  deriving (Eq, Show)


-- | Attempts to create a 'EmailAddress' value with the given text.
--
-- >>> mkEmailAddress ""
-- Left "Invalid email address: \": not enough input"
-- >>> mkEmailAddress "a"
-- Left "Invalid email address: at sign > @: not enough input"
-- >>> mkEmailAddress "a@"
-- Left "Invalid email address: [: not enough input"
-- >>> mkEmailAddress "@com"
-- Left "Invalid email address: \": Failed reading: satisfyWith"
-- >>> mkEmailAddress "a@localhost"
-- Right (MkEmailAddress {unEmailAddress = "a@localhost"})
-- >>> mkEmailAddress "spaces. are. allowed@example.com"
-- Right (MkEmailAddress {unEmailAddress = "spaces.are.allowed@example.com"})
mkEmailAddress :: T.Text -> Either T.Text EmailAddress
mkEmailAddress x = case Email.validate (TE.encodeUtf8 x) of
  Left s -> Left ("Invalid email address: " <> T.pack s)
  Right ea -> pure (MkEmailAddress (TE.decodeUtf8 (Email.toByteString ea)))
