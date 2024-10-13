{-# LANGUAGE DeriveAnyClass #-}

module Brave.Summarizer (Summarizer (..)) where

import Data.Aeson
import Data.Text
import GHC.Generics (Generic)

data Summarizer = Summarizer
    {key :: Text}
    deriving (Show, Generic, FromJSON, ToJSON)

-- instance FromJSON Summarizer where
--     parseJSON =
--         genericParseJSON
--             defaultOptions
--                 { rejectUnknownFields = True
--                 }
-- <> "summary" =: ("1" :: Text)
