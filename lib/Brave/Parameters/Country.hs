{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Brave.Parameters.Country (Country (..)) where

import Prelude qualified

import GHC.Generics (Generic)

import Data.Aeson
import Data.Char (toLower)
import Data.Text (Text)

import Web.HttpApiData (ToHttpApiData (..))

-- https://api.search.brave.com/app/documentation/web-search/codes#country-codes
data Country
    = ALL
    | AR
    | AT
    | AU
    | BE
    | BR
    | CA
    | CH
    | CL
    | CN
    | DE
    | DK
    | ES
    | FI
    | FR
    | GB
    | HK
    | ID
    | IN
    | IT
    | JP
    | KR
    | MX
    | MY
    | NL
    | NO
    | NZ
    | PH
    | PL
    | PT
    | RU
    | SA
    | SE
    | TR
    | TW
    | US
    | ZA
    deriving (Prelude.Show, Prelude.Eq, Generic, ToJSON)

instance FromJSON Country where
    parseJSON =
        genericParseJSON
            defaultOptions
                { constructorTagModifier = Prelude.map toLower
                }

instance ToHttpApiData Country where
    toQueryParam :: Country -> Text
    toQueryParam ALL = "all"
    toQueryParam AR = "ar"
    toQueryParam AT = "at"
    toQueryParam AU = "au"
    toQueryParam BE = "be"
    toQueryParam BR = "br"
    toQueryParam CA = "ca"
    toQueryParam CH = "ch"
    toQueryParam CL = "cl"
    toQueryParam CN = "cn"
    toQueryParam DE = "de"
    toQueryParam DK = "dk"
    toQueryParam ES = "es"
    toQueryParam FI = "fi"
    toQueryParam FR = "fr"
    toQueryParam GB = "gb"
    toQueryParam HK = "hk"
    toQueryParam ID = "id"
    toQueryParam IN = "in"
    toQueryParam IT = "it"
    toQueryParam JP = "jp"
    toQueryParam KR = "kr"
    toQueryParam MX = "mx"
    toQueryParam MY = "my"
    toQueryParam NL = "nl"
    toQueryParam NO = "no"
    toQueryParam NZ = "nz"
    toQueryParam PH = "ph"
    toQueryParam PL = "pl"
    toQueryParam PT = "pt"
    toQueryParam RU = "ru"
    toQueryParam SA = "sa"
    toQueryParam SE = "se"
    toQueryParam TR = "tr"
    toQueryParam TW = "tw"
    toQueryParam US = "us"
    toQueryParam ZA = "za"
