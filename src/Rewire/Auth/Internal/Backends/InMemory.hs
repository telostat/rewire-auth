{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Rewire.Auth.Internal.Backends.InMemory where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (gets)
import Control.Monad.State.Strict (MonadState, StateT, modify')
import Data.Foldable (find)
import qualified Data.Text as T
import Data.UUID (UUID)
import qualified Data.UUID.V4 as UUID.V4
import Rewire.Auth.Internal.Class (MonadUser (..))
import Rewire.Auth.Internal.Exceptions (throwCanNotCreateUser, throwCanNotSetPassword)
import Rewire.Auth.Internal.Password (Password)
import Rewire.Auth.Internal.User (
  User (..),
  UserCreateForm (UserCreateForm),
  UserCredentials (UserCredentials),
  UserIdentQuery (..),
 )
import Rewire.Auth.Internal.Username (mkUsername)


data InMemoryBackendState = InMemoryBackendState
  { inMemoryBackendStateUsers :: ![User]
  , inMemoryBackendStatePasswords :: ![(UUID, Password)]
  }


newtype InMemoryBackend m = InMemoryBackendT
  { unInMemoryBackendT :: StateT InMemoryBackendState IO m
  }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadState InMemoryBackendState)


instance MonadUser InMemoryBackend where
  findUser :: UserIdentQuery -> InMemoryBackend (Maybe User)
  findUser query = do
    users <- gets inMemoryBackendStateUsers
    pure $ case query of
      UserIdentQueryById x -> find (\u -> userId u == x) users
      UserIdentQueryByName x -> find (\u -> userName u == x) users
      UserIdentQueryByMail x -> find (\u -> userMail u == x) users


  findUserByCredentials :: UserCredentials -> InMemoryBackend (Maybe User)
  findUserByCredentials (UserCredentials i p) = do
    mUser <- findUser i
    case mUser of
      Nothing -> pure Nothing
      Just su -> do
        let uid = userId su
        passwords <- gets inMemoryBackendStatePasswords
        case find (\(pu, pp) -> uid == pu && p == pp) passwords of
          Nothing -> pure Nothing
          Just _ -> pure (Just su)


  createUser :: UserCreateForm -> InMemoryBackend User
  createUser (UserCreateForm m mn) = do
    mUserForMail <- findUser (UserIdentQueryByMail m)
    mUserForName <- case mn of
      Nothing -> pure Nothing
      Just sn -> findUser (UserIdentQueryByName sn)
    case (mUserForMail, mUserForName) of
      (Nothing, Nothing) -> go
      _ -> throwCanNotCreateUser "A user with same email address or username already exists!"
    where
      go = do
        uid <- liftIO UUID.V4.nextRandom
        name <- case mn of
          Nothing -> case mkUsername (T.pack $ show uid) of
            Left err -> throwCanNotCreateUser ("Error while creating the missing username: " <> err)
            Right sn -> pure sn
          Just user -> pure user
        let user =
              User
                { userId = uid
                , userName = name
                , userMail = m
                }
        modify' (\s -> s {inMemoryBackendStateUsers = user : inMemoryBackendStateUsers s})
        pure user


  setUserPassword :: UserIdentQuery -> Password -> InMemoryBackend ()
  setUserPassword query password = do
    mUser <- findUser query
    case mUser of
      Nothing -> throwCanNotSetPassword "No such user found to set its password."
      Just su -> modify' (\s -> s {inMemoryBackendStatePasswords = (userId su, password) : inMemoryBackendStatePasswords s})
