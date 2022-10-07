-- | This module provides "Data.Aeson" helpers.
module Rewire.Auth.Internal.Aeson where

import qualified Data.Aeson as Aeson
import qualified Data.List as List


-- | Custom common 'Aeson.Options' used across this library/appliaction.
customAesonOptions :: String -> Aeson.Options
customAesonOptions prefix =
  Aeson.defaultOptions
    { Aeson.fieldLabelModifier = dropPrefixSnake
    , Aeson.constructorTagModifier = dropPrefixSnake
    , Aeson.sumEncoding =
        Aeson.TaggedObject
          { Aeson.tagFieldName = "type"
          , Aeson.contentsFieldName = "value"
          }
    }
  where
    dropPrefixSnake x = maybe x (Aeson.camelTo2 '_') (List.stripPrefix prefix x)
