{-# LANGUAGE DeriveAnyClass #-}

module Brave.Responses.Search (Search (..)) where

import GHC.Generics (Generic)

import Data.Aeson

import Brave.Responses.SearchResult (SearchResult (..))

data Search = Search
    { results :: [SearchResult]
    , family_friendly :: Bool
    }
    deriving (Show, Generic, ToJSON)

instance FromJSON Search where
    parseJSON =
        genericParseJSON
            defaultOptions

-- { rejectUnknownFields = True
-- }
