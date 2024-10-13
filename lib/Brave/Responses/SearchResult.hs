{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Brave.Responses.SearchResult (SearchResult (..)) where

import GHC.Generics (Generic)

import Data.Aeson
import Data.Text

import Brave.Responses.MetaUrl (MetaUrl (..))
import Brave.Responses.Thumbnail (Thumbnail (..))

data SearchResult = SearchResult
    { title :: Text
    , url :: Text
    , schemas :: Maybe [Object]
    , meta_url :: MetaUrl
    , thumbnail :: Maybe Thumbnail
    -- , isSourceLocal :: Bool
    -- , isSourceBoth :: Bool
    -- , description :: Text
    -- -- , pageAge :: UTCTime
    -- , profile :: Profile
    -- , language :: Text
    -- -- , familyFriendly :: Bool
    -- , type_ :: Text
    -- , subtype :: Text
    -- , metaUrl :: MetaUrl
    -- , age :: Text
    }
    deriving (Show, Generic, FromJSON, ToJSON)

-- instance FromJSON SearchResult where
--     parseJSON =
--         genericParseJSON
--             defaultOptions
--                 { rejectUnknownFields = True
--                 }
