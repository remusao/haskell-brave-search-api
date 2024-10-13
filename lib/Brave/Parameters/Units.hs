{-# LANGUAGE OverloadedStrings #-}

module Brave.Parameters.Units (Units (..)) where

import Data.Text

import Web.HttpApiData (ToHttpApiData (..))

data Units = UnitsMetric | UnitsImperial deriving (Show, Eq)

instance ToHttpApiData Units where
    toQueryParam :: Units -> Text
    toQueryParam UnitsMetric = "metric"
    toQueryParam UnitsImperial = "imperial"
