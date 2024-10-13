{-# LANGUAGE OverloadedStrings #-}

module Brave.Parameters.SafeSearch (SafeSearch (..)) where

import Data.Text

import Web.HttpApiData (ToHttpApiData (..))

data SafeSearch = Off | Moderate | Strict deriving (Show, Eq)

instance ToHttpApiData SafeSearch where
    toQueryParam :: SafeSearch -> Text
    toQueryParam Off = "off"
    toQueryParam Moderate = "moderate"
    toQueryParam Strict = "strict"
