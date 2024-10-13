{-# LANGUAGE OverloadedStrings #-}

module Brave.Parameters.Freshness (Freshness (..)) where

-- import Data.Time.Clock (UTCTime)
import Data.Text

import Web.HttpApiData (ToHttpApiData (..))

data Freshness
    = FreshnessPastDay
    | FreshnessPastWeek
    | FreshnessPastMonth
    | FreshnessPastYear
    deriving (Show, Eq)

-- TODO

-- | FreshnessRang UTCTime UTCTime
instance ToHttpApiData Freshness where
    toQueryParam :: Freshness -> Text
    toQueryParam FreshnessPastDay = "pd"
    toQueryParam FreshnessPastWeek = "pw"
    toQueryParam FreshnessPastMonth = "pm"
    toQueryParam FreshnessPastYear = "pw"
