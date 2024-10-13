{-# LANGUAGE OverloadedStrings #-}

module Brave.Parameters.ResultFilter (ResultFilter (..)) where

import Data.Text

import Web.HttpApiData (ToHttpApiData (..))

data ResultFilter
    = ResultFilterDiscussions
    | ResultFilterFAQ
    | ResultFilterInfobox
    | ResultFilterNews
    | ResultFilterVideos
    | ResultFilterQuery
    | ResultFilterSummarizer
    | ResultFilterWeb
    | ResultFilterLocations
    deriving (Show, Eq)

instance ToHttpApiData ResultFilter where
    toQueryParam :: ResultFilter -> Text
    toQueryParam ResultFilterDiscussions = "discussions"
    toQueryParam ResultFilterFAQ = "faq"
    toQueryParam ResultFilterInfobox = "infobox"
    toQueryParam ResultFilterNews = "news"
    toQueryParam ResultFilterVideos = "videos"
    toQueryParam ResultFilterQuery = "query"
    toQueryParam ResultFilterSummarizer = "summarizer"
    toQueryParam ResultFilterWeb = "web"
    toQueryParam ResultFilterLocations = "locations"

instance ToHttpApiData [ResultFilter] where
    toQueryParam :: [ResultFilter] -> Text
    toQueryParam = intercalate "," . fmap toQueryParam
