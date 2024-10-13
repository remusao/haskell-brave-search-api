{-# LANGUAGE DeriveAnyClass #-}

module Brave.Responses.Thumbnail (Thumbnail (..)) where

import GHC.Generics (Generic)

import Data.Aeson
import Data.Text

data Thumbnail = Thumbnail
    { src :: Text
    , original :: Text
    , logo :: Bool -- TODO should be indicated in docs
    }
    deriving (Show, Generic, ToJSON)

instance FromJSON Thumbnail where
    parseJSON =
        genericParseJSON
            defaultOptions
                { rejectUnknownFields = True
                }
