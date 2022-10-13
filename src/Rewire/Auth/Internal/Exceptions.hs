{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Rewire.Auth.Internal.Exceptions where

import Control.Exception (Exception)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Catch.Pure (MonadThrow (..))
import qualified Data.List as List
import qualified Data.Text as T
import GHC.Exception (prettyCallStack)
import GHC.Stack (HasCallStack, callStack)
import Rewire.Auth.Internal.User (UserIdentQuery)


-- | Type encoding for exceptions this module can throw.
data RewireAuthException where
  RewireAuthExceptionNoSuchUser :: HasCallStack => UserIdentQuery -> RewireAuthException
  RewireAuthExceptionCanNotCreateUser :: HasCallStack => T.Text -> RewireAuthException
  RewireAuthExceptionCanNotSetPassword :: HasCallStack => T.Text -> RewireAuthException
  RewireAuthExceptionUnknownError :: HasCallStack => T.Text -> RewireAuthException
  RewireAuthExceptionWrappedError :: HasCallStack => T.Text -> RewireAuthException -> RewireAuthException


instance Exception RewireAuthException


instance Show RewireAuthException where
  show = exceptionToString 0


exceptionToString :: HasCallStack => Int -> RewireAuthException -> String
exceptionToString is exception = case exception of
  RewireAuthExceptionNoSuchUser usrid -> indent is $ prefixMessage (T.pack (show usrid)) <> stackTrace
  RewireAuthExceptionUnknownError txt -> indent is $ prefixMessage txt <> stackTrace
  RewireAuthExceptionCanNotCreateUser txt -> indent is $ prefixMessage txt <> stackTrace
  RewireAuthExceptionCanNotSetPassword txt -> indent is $ prefixMessage txt <> stackTrace
  RewireAuthExceptionWrappedError txt exc -> prefixMessage txt <> stackTrace <> "\nUnderlying Exception:\n" <> exceptionToString (is + 1) exc
  where
    indent i x = List.intercalate "\n" $ fmap (replicate (i * 4) ' ' <>) (lines x)
    prefix = "Rewire Auth Exception: "
    prefixMessage = (<> ".") . (<>) prefix . T.unpack

    stackTrace :: HasCallStack => String
    stackTrace = "\nStack Trace:\n" <> indent 1 (prettyCallStack callStack)


throwNoSuchUser :: MonadThrow m => UserIdentQuery -> m a
throwNoSuchUser usrid = throwM (RewireAuthExceptionNoSuchUser usrid)


throwCanNotCreateUser :: MonadThrow m => T.Text -> m a
throwCanNotCreateUser err = throwM (RewireAuthExceptionCanNotCreateUser err)


throwCanNotSetPassword :: MonadThrow m => T.Text -> m a
throwCanNotSetPassword err = throwM (RewireAuthExceptionCanNotSetPassword err)
