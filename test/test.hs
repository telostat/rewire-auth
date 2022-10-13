{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad (when)
import Control.Monad.Catch (MonadCatch (catch), MonadThrow (throwM))
import Control.Monad.IO.Class (MonadIO)
import Data.Maybe (isJust)
import qualified Data.Text as T
import Rewire.Auth.Internal.Backends.InMemory (
  runInMemoryBackendWithEmptyState,
 )
import Rewire.Auth.Internal.Class (MonadUser (..))
import Rewire.Auth.Internal.EmailAddress (mkEmailAddress)
import Rewire.Auth.Internal.Exceptions (RewireAuthException (..))
import Rewire.Auth.Internal.Password (mkPassword)
import Rewire.Auth.Internal.User (
  User (userId, userMail, userName),
  UserCreateForm (..),
  UserCredentials (UserCredentials),
  UserIdentQuery (
    UserIdentQueryById,
    UserIdentQueryByMail,
    UserIdentQueryByName
  ),
 )
import Rewire.Auth.Internal.Username (mkUsername)


main :: IO ()
main = do
  runInMemoryBackendWithEmptyState testProgramM


testProgramM :: (MonadUser m, MonadIO m, MonadCatch m) => m ()
testProgramM = do
  user1 <- createUser (testUserForm "user1@localhost" Nothing)
  user2 <- createUser (testUserForm "user2@localhost" (Just "Alice"))

  testProgramDuplicateUserCreationM (testUserForm "user2@localhost" Nothing)
  testProgramDuplicateUserCreationM (testUserForm "user3@localhost" (Just "Alice"))

  let (Right password1) = mkPassword "a12345678"
  let (Right password2) = mkPassword "b12345678"
  let (Right password3) = mkPassword "c12345678"

  setUserPassword (UserIdentQueryById (userId user1)) password1
  setUserPassword (UserIdentQueryById (userId user2)) password2

  foundUser1 <- findUserByCredentials (UserCredentials (UserIdentQueryById (userId user1)) password1)
  when (foundUser1 /= Just user1) (error "findUserByCredentials has failed for user1")

  foundUser2 <- findUserByCredentials (UserCredentials (UserIdentQueryById (userId user2)) password2)
  when (foundUser2 /= Just user2) (error "findUserByCredentials has failed for user2")

  notFound <- findUserByCredentials (UserCredentials (UserIdentQueryById (userId user1)) password3)
  when (isJust notFound) (error "findUserByCredentials has failed for user1 with wrong credentials")

  setUserPassword (UserIdentQueryById (userId user1)) password3

  foundUser1' <- findUserByCredentials (UserCredentials (UserIdentQueryById (userId user1)) password3)
  when (foundUser1' /= Just user1) (error "findUserByCredentials has failed for user1")

  notFound' <- findUserByCredentials (UserCredentials (UserIdentQueryById (userId user1)) password1)
  when (isJust notFound') (error "findUserByCredentials found user1 with old credentials")

  user1' <- getUser (UserIdentQueryById (userId user1))
  user1'' <- getUser (UserIdentQueryByName (userName user1))
  user1''' <- getUser (UserIdentQueryByMail (userMail user1))
  when (user1' /= user1 || user1'' /= user1 || user1''' /= user1) (error "getUser failed for user1")

  user2' <- getUser (UserIdentQueryById (userId user2))
  user2'' <- getUser (UserIdentQueryByName (userName user2))
  user2''' <- getUser (UserIdentQueryByMail (userMail user2))
  when (user2' /= user2 || user2'' /= user2 || user2''' /= user2) (error "getUser failed for user2")

  mUser1' <- findUser (UserIdentQueryById (userId user1))
  mUser1'' <- findUser (UserIdentQueryByName (userName user1))
  mUser1''' <- findUser (UserIdentQueryByMail (userMail user1))
  when (mUser1' /= Just user1 || mUser1'' /= Just user1 || mUser1''' /= Just user1) (error "findUser failed for user1")

  mUser2' <- findUser (UserIdentQueryById (userId user2))
  mUser2'' <- findUser (UserIdentQueryByName (userName user2))
  mUser2''' <- findUser (UserIdentQueryByMail (userMail user2))
  when (mUser2' /= Just user2 || mUser2'' /= Just user2 || mUser2''' /= Just user2) (error "findUser failed for user2")


testProgramDuplicateUserCreationM :: (MonadUser m, MonadIO m, MonadCatch m) => UserCreateForm -> m ()
testProgramDuplicateUserCreationM form = do
  (createUser form >> error "Duplicate user creation should not have worked!") `catch` expectException
  where
    expectException (RewireAuthExceptionCanNotCreateUser _) = pure ()
    expectException exc = throwM exc


testUserForm :: T.Text -> Maybe T.Text -> UserCreateForm
testUserForm mail mName =
  let (Right usermail) = mkEmailAddress mail
      x = (forceRight . mkUsername <$> mName)
   in UserCreateForm
        { userCreateFormMail = usermail
        , userCreateFormName = x
        }


forceRight :: Either a b -> b
forceRight (Left _) = error "Can not extract right from left"
forceRight (Right x) = x
