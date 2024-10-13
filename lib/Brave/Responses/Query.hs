{-# LANGUAGE DeriveAnyClass #-}

module Brave.Responses.Query (Query (..)) where

import GHC.Generics (Generic)

import Data.Aeson
import Data.Text

import Brave.Parameters.Country (Country (..))
import Brave.Responses.Language (Language (..))

data Query = Query
    { original :: Text
    , show_strict_warning :: Bool
    , altered :: Maybe Text -- TODO should be indicated as optional in docs
    , safesearch :: Maybe Bool -- TODO should be indicated as optional in docs
    , is_navigational :: Bool
    , is_geolocal :: Maybe Bool -- TODO should be indicated as optional in docs
    , local_decision :: Maybe Text -- TODO enum + should be indicated as optional in docs
    , local_locations_idx :: Maybe Int -- TODO should be indicated as optional in docs
    , is_trending :: Maybe Bool -- TODO should be indicated as optional in docs
    , is_news_breaking :: Bool
    , ask_for_location :: Maybe Bool -- TODO should be indicated as optional in docs
    , language :: Maybe Language -- TODO should be indicated as optional in docs
    , spellcheck_off :: Bool
    , country :: Country -- TODO here we retrieve lower-case country whereas query param is upper-case
    , bad_results :: Bool
    , should_fallback :: Bool -- TODO makes no sense for API
    , lat :: Maybe Text -- TODO should be indicated as optional in docs
    , long :: Maybe Text -- TODO should be indicated as optional in docs
    , postal_code :: Text
    , city :: Text
    , state :: Text
    , header_country :: Text
    , more_results_available :: Bool
    , custom_location_label :: Maybe Text -- TODO should be indicated as optional in docs
    , reddit_cluster :: Maybe Text -- TODO should be indicated as optional in docs
    }
    deriving (Show, Eq, Generic, ToJSON)

instance FromJSON Query where
    parseJSON =
        genericParseJSON
            defaultOptions
                { rejectUnknownFields = True
                }
