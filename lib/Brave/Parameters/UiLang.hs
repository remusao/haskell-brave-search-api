{-# LANGUAGE OverloadedStrings #-}

module Brave.Parameters.UiLang (UiLang (..)) where

import Data.Text

import Web.HttpApiData (ToHttpApiData (..))

-- https://api.search.brave.com/app/documentation/web-search/codes#market-codes
data UiLang
    = DA_DK
    | DE_AT
    | DE_CH
    | DE_DE
    | EN_AU
    | EN_CA
    | EN_GB
    | EN_ID
    | EN_IN
    | EN_MY
    | EN_NZ
    | EN_PH
    | EN_US
    | EN_ZA
    | ES_AR
    | ES_CL
    | ES_ES
    | ES_MX
    | ES_US
    | FI_FI
    | FR_BE
    | FR_CA
    | FR_CH
    | FR_FR
    | IT_IT
    | JA_JP
    | KO_KR
    | NL_BE
    | NL_NL
    | NO_NO
    | PL_PL
    | PT_BR
    | RU_RU
    | SV_SE
    | TR_TR
    | ZH_CN
    | ZH_HK
    | ZH_TW
    deriving (Show, Eq)

instance ToHttpApiData UiLang where
    toQueryParam :: UiLang -> Text
    toQueryParam DA_DK = "da-DK"
    toQueryParam DE_AT = "de-AT"
    toQueryParam DE_CH = "de-CH"
    toQueryParam DE_DE = "de-DE"
    toQueryParam EN_AU = "en-AU"
    toQueryParam EN_CA = "en-CA"
    toQueryParam EN_GB = "en-GB"
    toQueryParam EN_ID = "en-ID"
    toQueryParam EN_IN = "en-IN"
    toQueryParam EN_MY = "en-MY"
    toQueryParam EN_NZ = "en-NZ"
    toQueryParam EN_PH = "en-PH"
    toQueryParam EN_US = "en-US"
    toQueryParam EN_ZA = "en-ZA"
    toQueryParam ES_AR = "es-AR"
    toQueryParam ES_CL = "es-CL"
    toQueryParam ES_ES = "es-ES"
    toQueryParam ES_MX = "es-MX"
    toQueryParam ES_US = "es-US"
    toQueryParam FI_FI = "fi-FI"
    toQueryParam FR_BE = "fr-BE"
    toQueryParam FR_CA = "fr-CA"
    toQueryParam FR_CH = "fr-CH"
    toQueryParam FR_FR = "fr-FR"
    toQueryParam IT_IT = "it-IT"
    toQueryParam JA_JP = "ja-JP"
    toQueryParam KO_KR = "ko-KR"
    toQueryParam NL_BE = "nl-BE"
    toQueryParam NL_NL = "nl-NL"
    toQueryParam NO_NO = "no-NO"
    toQueryParam PL_PL = "pl-PL"
    toQueryParam PT_BR = "pt-BR"
    toQueryParam RU_RU = "ru-RU"
    toQueryParam SV_SE = "sv-SE"
    toQueryParam TR_TR = "tr-TR"
    toQueryParam ZH_CN = "zh-CN"
    toQueryParam ZH_HK = "zh-HK"
    toQueryParam ZH_TW = "zh-TW"
