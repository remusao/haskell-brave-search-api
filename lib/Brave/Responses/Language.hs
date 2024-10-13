module Brave.Responses.Language (Language (..)) where

import GHC.Generics (Generic)

import Data.Aeson
import Data.Text

newtype Language = Language Text deriving (Show, Eq, Generic, ToJSON)

instance FromJSON Language where
    parseJSON =
        genericParseJSON
            defaultOptions
                { rejectUnknownFields = True
                }
