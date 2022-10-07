{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- | This module provides login endpoint implementation.
module Rewire.Auth.Web.Api.Endpoints.Auth.Login where

import Control.Lens ((&), (?~))
import qualified Control.Lens as Lens
import qualified Data.Aeson as Aeson
import qualified Data.OpenApi as OpenApi
import qualified Data.Text as T
import GHC.Generics (Generic)
import Rewire.Auth.Internal.Aeson (customAesonOptions)
import Rewire.Auth.Internal.Secret (Secret, mkSecret)
import Servant (type (:>))
import qualified Servant


-- $setup
--
-- >>> :set -XTypeApplications


-- * API Definition


-- | Login API definition.
type Api =
  "login"
    :> Servant.Summary "Login Endpoint"
    :> Servant.Description "This endpoint authenticates incoming login requests, opens a session and returns session information."
    :> Servant.ReqBody '[Servant.JSON] LoginRequestPayload
    :> Servant.Post '[Servant.JSON] LoginResponsePayload


-- | Login API handler.
handler :: Monad m => LoginRequestPayload -> m LoginResponsePayload
handler _ = pure $ LoginResponsePayloadSession "my-jwt"


-- * API Implementation


-- ** Request Payload


-- | Login API request payload definition.
data LoginRequestPayload = LoginRequestPayload
  { loginRequestPayloadIdent :: !T.Text
  -- ^ User identification.
  , loginRequestPayloadPassword :: !Secret
  -- ^ User password.
  }
  deriving (Eq, Generic, Show)


-- | 'Aeson.FromJSON' instance for 'LoginRequestPayload'.
--
-- >>> let example = LoginRequestPayload "my-ident" (mkSecret "my-password")
-- >>> example
-- LoginRequestPayload {loginRequestPayloadIdent = "my-ident", loginRequestPayloadPassword = <redacted>}
-- >>> let exampleEncoded = Aeson.encode example
-- >>> exampleEncoded
-- "{\"ident\":\"my-ident\",\"password\":\"my-password\"}"
-- >>> Aeson.decode @LoginRequestPayload exampleEncoded
-- Just (LoginRequestPayload {loginRequestPayloadIdent = "my-ident", loginRequestPayloadPassword = <redacted>})
-- >>> Aeson.decode @LoginRequestPayload exampleEncoded == Just example
-- True
instance Aeson.FromJSON LoginRequestPayload where
  parseJSON = Aeson.genericParseJSON (customAesonOptions "loginRequestPayload")


-- | 'Aeson.ToJSON' instance for 'LoginRequestPayload'.
--
-- >>> let example = LoginRequestPayload "my-ident" (mkSecret "my-password")
-- >>> Aeson.decode @LoginRequestPayload (Aeson.encode example) == (Just example)
-- True
instance Aeson.ToJSON LoginRequestPayload where
  toJSON = Aeson.genericToJSON (customAesonOptions "loginRequestPayload")


-- | 'OpenApi.ToSchema' instance for 'LoginRequestPayload'.
instance OpenApi.ToSchema LoginRequestPayload where
  declareNamedSchema proxy =
    OpenApi.genericDeclareNamedSchema (OpenApi.fromAesonOptions (customAesonOptions "loginRequestPayload")) proxy
      & Lens.mapped . OpenApi.schema . OpenApi.description ?~ "Login request payload"
      & Lens.mapped . OpenApi.schema . OpenApi.example ?~ Aeson.toJSON exampleLoginRequestPayload
    where
      exampleLoginRequestPayload :: LoginRequestPayload
      exampleLoginRequestPayload =
        LoginRequestPayload
          { loginRequestPayloadIdent = "user-id_or_username_or_email-address"
          , loginRequestPayloadPassword = mkSecret "password"
          }


-- ** Response Payload


-- | Login API response payload.
data LoginResponsePayload
  = -- | Response payload upon successful login.
    LoginResponsePayloadSession T.Text
  | -- | Response payload upon successful authentication attempt with
    --   next additional step for OTP authentication to complete login process.
    LoginResponsePayloadOtpReference T.Text
  deriving (Eq, Generic, Show)


-- | 'Aeson.FromJSON' instance for 'LoginResponsePayload'.
--
-- >>> let exampleSession = LoginResponsePayloadSession "my-jwt"
-- >>> exampleSession
-- LoginResponsePayloadSession "my-jwt"
-- >>> let exampleSessionEncoded = Aeson.encode exampleSession
-- >>> exampleSessionEncoded
-- "{\"type\":\"session\",\"value\":\"my-jwt\"}"
-- >>> Aeson.decode @LoginResponsePayload exampleSessionEncoded
-- Just (LoginResponsePayloadSession "my-jwt")
-- >>> Aeson.decode @LoginResponsePayload exampleSessionEncoded == Just exampleSession
-- True
--
-- >>> let exampleOtpReference = LoginResponsePayloadOtpReference "my-otp-reference"
-- >>> exampleOtpReference
-- LoginResponsePayloadOtpReference "my-otp-reference"
-- >>> let exampleOtpReferenceEncoded = Aeson.encode exampleOtpReference
-- >>> exampleOtpReferenceEncoded
-- "{\"type\":\"otp_reference\",\"value\":\"my-otp-reference\"}"
-- >>> Aeson.decode @LoginResponsePayload exampleOtpReferenceEncoded
-- Just (LoginResponsePayloadOtpReference "my-otp-reference")
-- >>> Aeson.decode @LoginResponsePayload exampleOtpReferenceEncoded == Just exampleOtpReference
-- True
instance Aeson.FromJSON LoginResponsePayload where
  parseJSON = Aeson.genericParseJSON (customAesonOptions "LoginResponsePayload")


-- | 'Aeson.ToJSON' instance for 'LoginResponsePayload'.
--
--
-- >>> let exampleSession = LoginResponsePayloadSession "my-jwt"
-- >>> Aeson.decode @LoginResponsePayload (Aeson.encode exampleSession) == (Just exampleSession)
-- True
--
-- >>> let exampleOtpReference = LoginResponsePayloadOtpReference "my-otp-reference"
-- >>> Aeson.decode @LoginResponsePayload (Aeson.encode exampleOtpReference) == (Just exampleOtpReference)
-- True
instance Aeson.ToJSON LoginResponsePayload where
  toJSON = Aeson.genericToJSON (customAesonOptions "LoginResponsePayload")


-- | 'OpenApi.ToSchema' instance for 'LoginResponsePayload'.
instance OpenApi.ToSchema LoginResponsePayload where
  declareNamedSchema proxy =
    OpenApi.genericDeclareNamedSchema (OpenApi.fromAesonOptions (customAesonOptions "LoginResponsePayload")) proxy
      & Lens.mapped . OpenApi.schema . OpenApi.description ?~ "Login response payload"
      & Lens.mapped . OpenApi.schema . OpenApi.example ?~ Aeson.toJSON exampleLoginResponsePayload
    where
      exampleLoginResponsePayload :: LoginResponsePayload
      exampleLoginResponsePayload = LoginResponsePayloadSession "my-jwt"
