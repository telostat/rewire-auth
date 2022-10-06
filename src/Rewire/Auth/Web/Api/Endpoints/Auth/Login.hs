{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- | This module provides login endpoint implementation.
module Rewire.Auth.Web.Api.Endpoints.Auth.Login where

import Control.Lens ((&), (?~))
import qualified Control.Lens as Lens
import qualified Data.Aeson as Aeson
import qualified Data.List as List
import qualified Data.OpenApi as OpenApi
import qualified Data.Text as T
import GHC.Generics (Generic)
import Servant (type (:>))
import qualified Servant


type ApiAuthLogin =
  "login"
    :> Servant.Summary "Login Endpoint"
    :> Servant.Description "This endpoint authenticates incoming login requests, opens a session and returns session information."
    :> Servant.ReqBody '[Servant.JSON] LoginRequestPayload
    :> Servant.Post '[Servant.JSON] LoginResponsePayload


apiAuthLogin :: Servant.Proxy ApiAuthLogin
apiAuthLogin = Servant.Proxy


data LoginRequestPayload = LoginRequestPayload
  { loginRequestPayloadIdent :: !T.Text
  , loginRequestPayloadPassword :: !Secret
  }
  deriving (Eq, Generic, Show)


-- >>> :set -XTypeApplications
-- >>> Aeson.decode @LoginRequestPayload "{\"ident\": \"my-ident\", \"password\": \"my-password\"}"
-- Just (LoginRequestPayload {loginRequestPayloadIdent = "my-ident", loginRequestPayloadPassword = <redacted>})
instance Aeson.FromJSON LoginRequestPayload where
  parseJSON = Aeson.genericParseJSON (customAesonOptions "loginRequestPayload")


-- >>> :set -XTypeApplications
-- >>> let example = LoginRequestPayload "my-ident" (Secret "my-password")
-- >>> Aeson.encode example
-- "{\"ident\":\"my-ident\",\"password\":\"my-password\"}"
-- >>> Aeson.decode @LoginRequestPayload (Aeson.encode example) == (Just example)
-- True
instance Aeson.ToJSON LoginRequestPayload where
  toJSON = Aeson.genericToJSON (customAesonOptions "loginRequestPayload")


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
          , loginRequestPayloadPassword = Secret "password"
          }


data LoginResponsePayload
  = LoginResponsePayloadSession T.Text
  | LoginResponsePayloadOtpReference T.Text
  deriving (Generic)


instance Aeson.FromJSON LoginResponsePayload where
  parseJSON = Aeson.genericParseJSON (customAesonOptions "LoginResponsePayload")


-- >>> Aeson.encode (LoginResponsePayloadSession "my-jwt")
-- "{\"type\":\"session\",\"value\":\"my-jwt\"}"
-- >>> Aeson.encode (LoginResponsePayloadOtpReference "my-otp-reference")
-- "{\"type\":\"otp_reference\",\"value\":\"my-otp-reference\"}"
instance Aeson.ToJSON LoginResponsePayload where
  toJSON = Aeson.genericToJSON (customAesonOptions "LoginResponsePayload")


instance OpenApi.ToSchema LoginResponsePayload where
  declareNamedSchema proxy =
    OpenApi.genericDeclareNamedSchema (OpenApi.fromAesonOptions (customAesonOptions "LoginResponsePayload")) proxy
      & Lens.mapped . OpenApi.schema . OpenApi.description ?~ "Login response payload"
      & Lens.mapped . OpenApi.schema . OpenApi.example ?~ Aeson.toJSON exampleLoginResponsePayload
    where
      exampleLoginResponsePayload :: LoginResponsePayload
      exampleLoginResponsePayload = LoginResponsePayloadSession "my-jwt"


performAuthLogin :: Monad m => LoginRequestPayload -> m LoginResponsePayload
performAuthLogin _ = pure $ LoginResponsePayloadSession "my-jwt"


-- * Helper Definitions


newtype Secret = Secret
  { unSecret :: T.Text
  }
  deriving (Eq, Aeson.FromJSON, Aeson.ToJSON, OpenApi.ToSchema)


instance Show Secret where
  show _ = "<redacted>"


customAesonOptions :: String -> Aeson.Options
customAesonOptions prefix =
  Aeson.defaultOptions
    { Aeson.fieldLabelModifier = dropPrefixSnake
    , Aeson.constructorTagModifier = dropPrefixSnake
    , Aeson.sumEncoding = Aeson.TaggedObject {Aeson.tagFieldName = "type", Aeson.contentsFieldName = "value"}
    }
  where
    dropPrefixSnake x = maybe x (Aeson.camelTo2 '_') (List.stripPrefix prefix x)
