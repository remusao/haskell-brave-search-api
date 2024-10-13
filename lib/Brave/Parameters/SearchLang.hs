{-# LANGUAGE OverloadedStrings #-}

module Brave.Parameters.SearchLang (SearchLang (..)) where

import Prelude (Eq, Show)

import Data.Text

import Web.HttpApiData (ToHttpApiData (..))

-- https://api.search.brave.com/app/documentation/web-search/codes#language-codes
data SearchLang
    = AR
    | BG
    | BN
    | CA
    | CS
    | DA
    | DE
    | EN
    | EN_GB
    | ES
    | ET
    | EU
    | FI
    | FR
    | GL
    | GU
    | HE
    | HI
    | HR
    | HU
    | IS
    | IT
    | JA
    | KN
    | KO
    | LT
    | LV
    | ML
    | MR
    | MS
    | NB
    | NL
    | PA
    | PL
    | PT_BR
    | PT_PT
    | RO
    | RU
    | SK
    | SL
    | SR
    | SV
    | TA
    | TE
    | TH
    | TR
    | UK
    | VI
    | ZH_HANS
    | ZH_HANT
    deriving (Show, Eq)

instance ToHttpApiData SearchLang where
    toQueryParam :: SearchLang -> Text
    toQueryParam AR = "ar"
    toQueryParam BG = "bg"
    toQueryParam BN = "bn"
    toQueryParam CA = "ca"
    toQueryParam CS = "cs"
    toQueryParam DA = "da"
    toQueryParam DE = "de"
    toQueryParam EN = "en"
    toQueryParam EN_GB = "en-GB"
    toQueryParam ES = "es"
    toQueryParam ET = "et"
    toQueryParam EU = "eu"
    toQueryParam FI = "fi"
    toQueryParam FR = "fr"
    toQueryParam GL = "gl"
    toQueryParam GU = "gu"
    toQueryParam HE = "he"
    toQueryParam HI = "hi"
    toQueryParam HR = "hr"
    toQueryParam HU = "hu"
    toQueryParam IS = "is"
    toQueryParam IT = "it"
    toQueryParam JA = "ja"
    toQueryParam KN = "kn"
    toQueryParam KO = "ko"
    toQueryParam LT = "lt"
    toQueryParam LV = "lv"
    toQueryParam ML = "ml"
    toQueryParam MR = "mr"
    toQueryParam MS = "ms"
    toQueryParam NB = "nb"
    toQueryParam NL = "nl"
    toQueryParam PA = "pa"
    toQueryParam PL = "pl"
    toQueryParam PT_BR = "pt-BR"
    toQueryParam PT_PT = "pt-PT"
    toQueryParam RO = "ro"
    toQueryParam RU = "ru"
    toQueryParam SK = "sk"
    toQueryParam SL = "sl"
    toQueryParam SR = "sr"
    toQueryParam SV = "sv"
    toQueryParam TA = "ta"
    toQueryParam TE = "te"
    toQueryParam TH = "th"
    toQueryParam TR = "tr"
    toQueryParam UK = "uk"
    toQueryParam VI = "vi"
    toQueryParam ZH_HANS = "zh-Hans"
    toQueryParam ZH_HANT = "zh-Hant"
