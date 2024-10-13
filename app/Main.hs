{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Console.CmdArgs

import Data.Aeson.Encode.Pretty qualified as Aeson (encodePretty)
import Data.ByteString qualified as ByteString (putStr)
import Data.ByteString.Char8 qualified as ByteString.Char8 (pack)
import Data.ByteString.Lazy qualified as ByteString.Lazy (toStrict)
import Data.Text qualified as Text (pack)

import Brave.Auth qualified (SubscriptionToken (..))
import Brave.Parameters.Country qualified as Country (Country (..))
import Brave.Parameters.SearchLang qualified as SearchLang (SearchLang (..))
import Brave.Web qualified (Params (..), defaultParams, searchWithParams)

data Cli = Cli
    { token :: String
    , query :: String
    }
    deriving (Show, Data, Typeable)

cli :: Cli
cli =
    Cli
        { token = def &= help "Your API key (https://api.search.brave.com/app/keys)"
        , query = def &= help "Your query"
        }

main :: IO ()
main = do
    args <- cmdArgs cli
    let params = Brave.Web.defaultParams{Brave.Web.country = Just Country.FR, Brave.Web.search_lang = Just SearchLang.FR}
    let searchSession = Brave.Web.searchWithParams params $ Brave.Auth.SubscriptionToken (ByteString.Char8.pack (token args))
    searchResults <- searchSession (Text.pack (query args))
    ByteString.putStr . ByteString.Lazy.toStrict $ Aeson.encodePretty searchResults
