{-# LANGUAGE DeriveAnyClass #-}

module Brave.Responses.MetaUrl (MetaUrl (..)) where

import GHC.Generics (Generic)

import Data.Aeson
import Data.Text

data MetaUrl = MetaUrl
    { scheme :: Text
    , netloc :: Text
    , hostname :: Text
    , favicon :: Text
    , path :: Text
    }
    deriving (Show, Generic, ToJSON)

instance FromJSON MetaUrl where
    parseJSON =
        genericParseJSON
            defaultOptions
                { rejectUnknownFields = True
                }
