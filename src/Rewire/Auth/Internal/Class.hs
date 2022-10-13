module Rewire.Auth.Internal.Class where

import Control.Monad.Catch (MonadThrow)
import Rewire.Auth.Internal.Exceptions (throwNoSuchUser)
import Rewire.Auth.Internal.Password (Password)
import Rewire.Auth.Internal.User


class MonadThrow m => MonadUser m where
  findUser :: UserIdentQuery -> m (Maybe User)


  getUser :: UserIdentQuery -> m User
  getUser query = do
    mUser <- findUser query
    case mUser of
      Nothing -> throwNoSuchUser query
      Just su -> pure su


  findUserByCredentials :: UserCredentials -> m (Maybe User)


  createUser :: UserCreateForm -> m User


  setUserPassword :: UserIdentQuery -> Password -> m ()
