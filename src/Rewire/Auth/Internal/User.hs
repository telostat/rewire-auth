module Rewire.Auth.Internal.User where

import Data.UUID (UUID)
import Rewire.Auth.Internal.EmailAddress (EmailAddress)
import Rewire.Auth.Internal.Password (Password)
import Rewire.Auth.Internal.Username (Username)


data User = User
  { userId :: !UUID
  , userName :: !Username
  , userMail :: !EmailAddress
  }
  deriving (Eq, Show)


data UserIdentQuery
  = UserIdentQueryById !UUID
  | UserIdentQueryByName !Username
  | UserIdentQueryByMail !EmailAddress
  deriving (Show)


data UserCredentials = UserCredentials
  { userCredentialsIdent :: !UserIdentQuery
  , userCredentialsPassword :: !Password
  }


data UserCreateForm = UserCreateForm
  { userCreateFormMail :: !EmailAddress
  , userCreateFormName :: !(Maybe Username)
  }
