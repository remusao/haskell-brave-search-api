{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Brave.Web (
    search,
    searchWithParams,
    WebSearchApiResponse (..),
    Params (..),
    defaultParams,
) where

import Data.Aeson

import Data.Text (Text)

import Network.HTTP.Req

import GHC.Generics (Generic)

import Brave.Auth (SubscriptionToken (..))
import Brave.Parameters.Country (Country (..))
import Brave.Parameters.Freshness (Freshness (..))
import Brave.Parameters.ResultFilter (ResultFilter (..))
import Brave.Parameters.SafeSearch (SafeSearch (..))
import Brave.Parameters.SearchLang (SearchLang (..))
import Brave.Parameters.UiLang (UiLang (..))
import Brave.Parameters.Units (Units (..))

import Brave.Responses.Query qualified as Query (Query (..))
import Brave.Responses.Search qualified as Search (Search (..))

data Params = Params
    { country :: Maybe Country
    , search_lang :: Maybe SearchLang
    , ui_lang :: Maybe UiLang
    , count :: Maybe Int
    , offset :: Maybe Int
    , safesearch :: Maybe SafeSearch
    , freshness :: Maybe Freshness
    , text_decorations :: Maybe Bool
    , spellcheck :: Maybe Bool
    , result_filter :: Maybe [ResultFilter]
    , goggles_id :: Maybe Text
    , units :: Maybe Units
    , extra_snippets :: Maybe Bool
    , summary :: Maybe Bool
    }
    deriving (Show, Eq)

defaultParams :: Params
defaultParams =
    Params
        { country = Nothing
        , search_lang = Nothing
        , ui_lang = Nothing
        , count = Nothing
        , offset = Nothing
        , safesearch = Nothing
        , freshness = Nothing
        , text_decorations = Nothing
        , spellcheck = Nothing
        , result_filter = Nothing
        , goggles_id = Nothing
        , units = Nothing
        , extra_snippets = Nothing
        , summary = Nothing
        }

paramsToOptions :: Params -> Option scheme -> Option scheme
paramsToOptions (Params{country, search_lang, ui_lang, count, offset, safesearch, freshness, text_decorations, spellcheck, result_filter, goggles_id, units, extra_snippets, summary}) option =
    option
        <> mconcat
            [ maybe mempty ("country" =:) country
            , maybe mempty ("search_lang" =:) search_lang
            , maybe mempty ("ui_lang" =:) ui_lang
            , maybe mempty ("count" =:) count
            , maybe mempty ("offset" =:) offset
            , maybe mempty ("safesearch" =:) safesearch
            , maybe mempty ("freshness" =:) freshness
            , maybe mempty ("text_decorations" =:) text_decorations
            , maybe mempty ("spellcheck" =:) spellcheck
            , maybe mempty ("result_filter" =:) result_filter
            , maybe mempty ("goggles_id" =:) goggles_id
            , maybe mempty ("units" =:) units
            , maybe mempty ("extra_snippets" =:) extra_snippets
            , maybe mempty ("summary" =:) summary
            ]

-- paramOptins = concat $ fmap

searchWithParams :: Params -> SubscriptionToken -> Text -> IO WebSearchApiResponse
searchWithParams params (SubscriptionToken token) query = runReq defaultHttpConfig $ do
    -- One function—full power and flexibility, automatic retrying on timeouts
    -- and such, automatic connection sharing.
    r :: JsonResponse WebSearchApiResponse <-
        req
            GET -- method
            (https "api.search.brave.com" /: "res" /: "v1" /: "web" /: "search") -- safe by construction URL
            NoReqBody -- use built-in options or add your own
            jsonResponse -- specify how to interpret response
            (paramsToOptions params (header "Accept" "application/json" <> header "Accept-Encoding" "gzip" <> header "X-Subscription-Token" token <> "q" =: query))
    pure (responseBody r)

-- This function is the equivalent of the curl command above, but in Haskell and using the Req library
search :: SubscriptionToken -> Text -> IO WebSearchApiResponse
search = searchWithParams defaultParams

-- liftIO $ print (responseBody  r)

-- {
--   "query": {
--     "original": "brave search",
--     "show_strict_warning": false,
--     "is_navigational": true,
--     "is_news_breaking": false,
--     "spellcheck_off": true,
--     "country": "us",
--     "bad_results": false,
--     "should_fallback": false,
--     "postal_code": "",
--     "city": "",
--     "header_country": "",
--     "more_results_available": true,
--     "state": ""
--   },
--   "mixed": {
--     "type": "mixed",
--     "main": [
--       {
--         "type": "web",
--         "index": 0,
--         "all": false
--       },
--       {
--         "type": "web",
--         "index": 1,
--         "all": false
--       },
--       {
--         "type": "videos",
--         "all": true
--       },
--       {
--         "type": "web",
--         "index": 2,
--         "all": false
--       },
--       {
--         "type": "web",
--         "index": 3,
--         "all": false
--       },
--       {
--         "type": "web",
--         "index": 4,
--         "all": false
--       },
--       {
--         "type": "web",
--         "index": 5,
--         "all": false
--       },
--       {
--         "type": "web",
--         "index": 6,
--         "all": false
--       },
--       {
--         "type": "web",
--         "index": 7,
--         "all": false
--       },
--       {
--         "type": "web",
--         "index": 8,
--         "all": false
--       },
--       {
--         "type": "web",
--         "index": 9,
--         "all": false
--       },
--       {
--         "type": "web",
--         "index": 10,
--         "all": false
--       },
--       {
--         "type": "web",
--         "index": 11,
--         "all": false
--       },
--       {
--         "type": "web",
--         "index": 12,
--         "all": false
--       },
--       {
--         "type": "web",
--         "index": 13,
--         "all": false
--       },
--       {
--         "type": "web",
--         "index": 14,
--         "all": false
--       },
--       {
--         "type": "web",
--         "index": 15,
--         "all": false
--       },
--       {
--         "type": "web",
--         "index": 16,
--         "all": false
--       },
--       {
--         "type": "web",
--         "index": 17,
--         "all": false
--       },
--       {
--         "type": "web",
--         "index": 18,
--         "all": false
--       }
--     ],
--     "top": [],
--     "side": []
--   },
--   "type": "search",
--   "videos": {
--     "type": "videos",
--     "results": [
--       {
--         "type": "video_result",
--         "url": "https://www.youtube.com/watch?v=Uu1Gc3GM7CM",
--         "title": "Brave's Independent Search Engine Is Finally Good",
--         "description": "Enjoy the videos and music you love, upload original content, and share it all with friends, family, and the world on YouTube.",
--         "video": {},
--         "meta_url": {
--           "scheme": "https",
--           "netloc": "youtube.com",
--           "hostname": "www.youtube.com",
--           "favicon": "https://imgs.search.brave.com/Wg4wjE5SHAargkzePU3eSLmWgVz84BEZk1SjSglJK_U/rs:fit:32:32:1:0/g:ce/aHR0cDovL2Zhdmlj/b25zLnNlYXJjaC5i/cmF2ZS5jb20vaWNv/bnMvOTkyZTZiMWU3/YzU3Nzc5YjExYzUy/N2VhZTIxOWNlYjM5/ZGVjN2MyZDY4Nzdh/ZDYzMTYxNmI5N2Rk/Y2Q3N2FkNy93d3cu/eW91dHViZS5jb20v",
--           "path": "› watch"
--         },
--         "thumbnail": {
--           "src": "https://imgs.search.brave.com/HCxM1WU1K_835MaXzU1pVRmMXD6WmG1tlTMloouTQkw/rs:fit:200:200:1:0/g:ce/aHR0cHM6Ly9pLnl0/aW1nLmNvbS92aS9V/dTFHYzNHTTdDTS9o/cWRlZmF1bHQuanBn",
--           "original": "https://i.ytimg.com/vi/Uu1Gc3GM7CM/hqdefault.jpg"
--         }
--       },
--       {
--         "type": "video_result",
--         "url": "https://m.youtube.com/watch?v=ux4VacF3hxc&pp=ygUJI29zM2JyYXZl",
--         "title": "Brave Browser And Search Engine Keep Getting Better",
--         "description": "",
--         "video": {},
--         "meta_url": {
--           "scheme": "https",
--           "netloc": "m.youtube.com",
--           "hostname": "m.youtube.com",
--           "favicon": "https://imgs.search.brave.com/p0B86IaQr1YuFWzlQTuEltmxPAh7UzF2DpyN8i96T6M/rs:fit:32:32:1:0/g:ce/aHR0cDovL2Zhdmlj/b25zLnNlYXJjaC5i/cmF2ZS5jb20vaWNv/bnMvZTExOWJhZDk1/ZTUxZWUwYmE3NTg0/N2QzZjQwZjZhMTE5/ZTc1NWY5MzMwYjll/OTJhNTM3ZjdmZTMw/MzdlM2EwNC9tLnlv/dXR1YmUuY29tLw",
--           "path": "› watch"
--         },
--         "thumbnail": {
--           "src": "https://imgs.search.brave.com/CBnpTIOYfidcBKH0Y56b0dKxeb4GLCs4dgjTf60D0Ik/rs:fit:200:200:1:0/g:ce/aHR0cHM6Ly9pLnl0/aW1nLmNvbS92aS91/eDRWYWNGM2h4Yy9o/cWRlZmF1bHQuanBn",
--           "original": "https://i.ytimg.com/vi/ux4VacF3hxc/hqdefault.jpg"
--         }
--       },
--       {
--         "type": "video_result",
--         "url": "https://www.youtube.com/watch?v=IcRGa4DOR1I",
--         "title": "People Keep Asking Me Why I Use Brave Browser - YouTube",
--         "description": "I keep getting asked by viewers about why I use the Brave browser. What does Brave offer that keeps me using it as my default browser?REFERENCED:► https://b...",
--         "age": "September 9, 2024",
--         "page_age": "2024-09-09T13:00:31",
--         "video": {},
--         "meta_url": {
--           "scheme": "https",
--           "netloc": "youtube.com",
--           "hostname": "www.youtube.com",
--           "favicon": "https://imgs.search.brave.com/Wg4wjE5SHAargkzePU3eSLmWgVz84BEZk1SjSglJK_U/rs:fit:32:32:1:0/g:ce/aHR0cDovL2Zhdmlj/b25zLnNlYXJjaC5i/cmF2ZS5jb20vaWNv/bnMvOTkyZTZiMWU3/YzU3Nzc5YjExYzUy/N2VhZTIxOWNlYjM5/ZGVjN2MyZDY4Nzdh/ZDYzMTYxNmI5N2Rk/Y2Q3N2FkNy93d3cu/eW91dHViZS5jb20v",
--           "path": "› watch"
--         },
--         "thumbnail": {
--           "src": "https://imgs.search.brave.com/UwqELGKEPocqUGwoYFUCvRRWsmKm7Br79IhO3Pz8AEY/rs:fit:200:200:1:0/g:ce/aHR0cHM6Ly9pLnl0/aW1nLmNvbS92aS9J/Y1JHYTRET1IxSS9t/YXhyZXNkZWZhdWx0/LmpwZw",
--           "original": "https://i.ytimg.com/vi/IcRGa4DOR1I/maxresdefault.jpg"
--         }
--       },
--       {
--         "type": "video_result",
--         "url": "https://www.youtube.com/watch?v=ux4VacF3hxc",
--         "title": "Brave Browser And Search Engine Keep Getting Better - YouTube",
--         "description": "The Brave Browser and the Brave Search Engine have become really good in the last couple of releases. The browser now comes with features such as vertical t...",
--         "age": "June 4, 2023",
--         "page_age": "2023-06-04T13:00:41",
--         "video": {},
--         "meta_url": {
--           "scheme": "https",
--           "netloc": "youtube.com",
--           "hostname": "www.youtube.com",
--           "favicon": "https://imgs.search.brave.com/Wg4wjE5SHAargkzePU3eSLmWgVz84BEZk1SjSglJK_U/rs:fit:32:32:1:0/g:ce/aHR0cDovL2Zhdmlj/b25zLnNlYXJjaC5i/cmF2ZS5jb20vaWNv/bnMvOTkyZTZiMWU3/YzU3Nzc5YjExYzUy/N2VhZTIxOWNlYjM5/ZGVjN2MyZDY4Nzdh/ZDYzMTYxNmI5N2Rk/Y2Q3N2FkNy93d3cu/eW91dHViZS5jb20v",
--           "path": "› watch"
--         },
--         "thumbnail": {
--           "src": "https://imgs.search.brave.com/uRXgz7SybT4z8X4NNu5mwUJSRPJaeT0a1b-q3iXRLp4/rs:fit:200:200:1:0/g:ce/aHR0cHM6Ly9pLnl0/aW1nLmNvbS92aS91/eDRWYWNGM2h4Yy9t/YXhyZXNkZWZhdWx0/LmpwZw",
--           "original": "https://i.ytimg.com/vi/ux4VacF3hxc/maxresdefault.jpg"
--         }
--       },
--       {
--         "type": "video_result",
--         "url": "https://www.youtube.com/watch?v=bjcSpIY89ng",
--         "title": "Brave Search Is Surprisingly Good, But...",
--         "description": "Enjoy the videos and music you love, upload original content, and share it all with friends, family, and the world on YouTube.",
--         "video": {},
--         "meta_url": {
--           "scheme": "https",
--           "netloc": "youtube.com",
--           "hostname": "www.youtube.com",
--           "favicon": "https://imgs.search.brave.com/Wg4wjE5SHAargkzePU3eSLmWgVz84BEZk1SjSglJK_U/rs:fit:32:32:1:0/g:ce/aHR0cDovL2Zhdmlj/b25zLnNlYXJjaC5i/cmF2ZS5jb20vaWNv/bnMvOTkyZTZiMWU3/YzU3Nzc5YjExYzUy/N2VhZTIxOWNlYjM5/ZGVjN2MyZDY4Nzdh/ZDYzMTYxNmI5N2Rk/Y2Q3N2FkNy93d3cu/eW91dHViZS5jb20v",
--           "path": "› watch"
--         },
--         "thumbnail": {
--           "src": "https://imgs.search.brave.com/3a2iDN7TPpGquZdElx5JBSEDGxyrUPpT9V9v3ooSsv0/rs:fit:200:200:1:0/g:ce/aHR0cHM6Ly9pLnl0/aW1nLmNvbS92aS9i/amNTcElZODluZy9o/cWRlZmF1bHQuanBn",
--           "original": "https://i.ytimg.com/vi/bjcSpIY89ng/hqdefault.jpg"
--         }
--       }
--     ],
--     "mutated_by_goggles": false
--   },
--   "web": {
--     "type": "search",
--     "results": [
--       {
--         "title": "Private Search Engine - Brave Search",
--         "url": "https://search.brave.com/",
--         "is_source_local": false,
--         "is_source_both": false,
--         "description": "<strong>Brave</strong> <strong>Search</strong> doesn’t track you or your queries. It is a private, independent, and transparent <strong>search</strong> engine. The real alternative to Google. On mobile, desktop, and anywhere the web takes you.",
--         "page_age": "2023-04-27T00:00:00",
--         "profile": {
--           "name": "Brave Search",
--           "url": "https://search.brave.com/",
--           "long_name": "search.brave.com",
--           "img": "https://imgs.search.brave.com/sMhTiU139tPDpDZhoaFMlYxXpBdX0Jx2fryJBQI_Pt0/rs:fit:32:32:1:0/g:ce/aHR0cDovL2Zhdmlj/b25zLnNlYXJjaC5i/cmF2ZS5jb20vaWNv/bnMvN2FlOGU5YzUx/MDBkZTE1OTZiMDJj/Y2UzYzJjZDgyN2Vm/MjdhOTgyNDQyNGZj/ZWQ4OTViYmJmZDU5/NTYxN2M0ZS9zZWFy/Y2guYnJhdmUuY29t/Lw"
--         },
--         "language": "en",
--         "family_friendly": true,
--         "type": "search_result",
--         "subtype": "generic",
--         "meta_url": {
--           "scheme": "https",
--           "netloc": "search.brave.com",
--           "hostname": "search.brave.com",
--           "favicon": "https://imgs.search.brave.com/sMhTiU139tPDpDZhoaFMlYxXpBdX0Jx2fryJBQI_Pt0/rs:fit:32:32:1:0/g:ce/aHR0cDovL2Zhdmlj/b25zLnNlYXJjaC5i/cmF2ZS5jb20vaWNv/bnMvN2FlOGU5YzUx/MDBkZTE1OTZiMDJj/Y2UzYzJjZDgyN2Vm/MjdhOTgyNDQyNGZj/ZWQ4OTViYmJmZDU5/NTYxN2M0ZS9zZWFy/Y2guYnJhdmUuY29t/Lw",
--           "path": ""
--         },
--         "age": "April 27, 2023"
--       },
--       {
--         "title": "Brave Search - Wikipedia",
--         "url": "https://en.wikipedia.org/wiki/Brave_Search",
--         "is_source_local": false,
--         "is_source_both": false,
--         "description": "<strong>Brave</strong> <strong>Search</strong> is a <strong>search</strong> engine developed by <strong>Brave</strong> Software, Inc., and is the default <strong>search</strong> engine for the <strong>Brave</strong> web browser in certain countries. <strong>Brave</strong> <strong>Search</strong> is a <strong>search</strong> engine developed by <strong>Brave</strong> Software, Inc. and released in Beta in March 2021, following the acquisition of Tailcat, a ...",
--         "page_age": "2024-08-10T10:30:07",
--         "profile": {
--           "name": "Wikipedia",
--           "url": "https://en.wikipedia.org/wiki/Brave_Search",
--           "long_name": "en.wikipedia.org",
--           "img": "https://imgs.search.brave.com/m6XxME4ek8DGIUcEPCqjRoDjf2e54EwL9pQzyzogLYk/rs:fit:32:32:1:0/g:ce/aHR0cDovL2Zhdmlj/b25zLnNlYXJjaC5i/cmF2ZS5jb20vaWNv/bnMvNjQwNGZhZWY0/ZTQ1YWUzYzQ3MDUw/MmMzMGY3NTQ0ZjNj/NDUwMDk5ZTI3MWRk/NWYyNTM4N2UwOTE0/NTI3ZDQzNy9lbi53/aWtpcGVkaWEub3Jn/Lw"
--         },
--         "language": "en",
--         "family_friendly": true,
--         "type": "search_result",
--         "subtype": "generic",
--         "deep_results": {
--           "buttons": [
--             {
--               "type": "button_result",
--               "title": "History",
--               "url": "https://en.wikipedia.org/wiki/Brave_Search#History"
--             },
--             {
--               "type": "button_result",
--               "title": "Features",
--               "url": "https://en.wikipedia.org/wiki/Brave_Search#Features"
--             },
--             {
--               "type": "button_result",
--               "title": "Data collection",
--               "url": "https://en.wikipedia.org/wiki/Brave_Search#Data_collection"
--             }
--           ]
--         },
--         "meta_url": {
--           "scheme": "https",
--           "netloc": "en.wikipedia.org",
--           "hostname": "en.wikipedia.org",
--           "favicon": "https://imgs.search.brave.com/m6XxME4ek8DGIUcEPCqjRoDjf2e54EwL9pQzyzogLYk/rs:fit:32:32:1:0/g:ce/aHR0cDovL2Zhdmlj/b25zLnNlYXJjaC5i/cmF2ZS5jb20vaWNv/bnMvNjQwNGZhZWY0/ZTQ1YWUzYzQ3MDUw/MmMzMGY3NTQ0ZjNj/NDUwMDk5ZTI3MWRk/NWYyNTM4N2UwOTE0/NTI3ZDQzNy9lbi53/aWtpcGVkaWEub3Jn/Lw",
--           "path": "› wiki  › Brave_Search"
--         },
--         "age": "August 10, 2024"
--       },
--       {
--         "title": "The browser that puts you first | Brave",
--         "url": "https://brave.com/",
--         "is_source_local": false,
--         "is_source_both": false,
--         "description": "<strong>Brave</strong> <strong>Search</strong> is the private, independent <strong>search</strong> engine that’s default for most new users of the <strong>Brave</strong> browser. It’s also available in any other browser at <strong>search</strong>.<strong>brave</strong>.com. <strong>Brave</strong> <strong>Search</strong> serves fast, accurate results from its own independent index of the Web, and offers unique features like ...",
--         "page_age": "2024-10-09T00:00:00",
--         "profile": {
--           "name": "Brave",
--           "url": "https://brave.com/",
--           "long_name": "brave.com",
--           "img": "https://imgs.search.brave.com/vnilI-MeUh5_v9ywDuoJrW0Q4m2J3p50IzXyFgVktSA/rs:fit:32:32:1:0/g:ce/aHR0cDovL2Zhdmlj/b25zLnNlYXJjaC5i/cmF2ZS5jb20vaWNv/bnMvZTg1MzQ2NjVk/ZGY5NDczMmYzN2I4/MjYwN2Y4MDdlMDlj/ZjJlZTM3ZjMxOWVl/MTQ0MDFlZDk0MjZh/MWQ3OGU3MC9icmF2/ZS5jb20v"
--         },
--         "language": "en",
--         "family_friendly": true,
--         "type": "search_result",
--         "subtype": "generic",
--         "meta_url": {
--           "scheme": "https",
--           "netloc": "brave.com",
--           "hostname": "brave.com",
--           "favicon": "https://imgs.search.brave.com/vnilI-MeUh5_v9ywDuoJrW0Q4m2J3p50IzXyFgVktSA/rs:fit:32:32:1:0/g:ce/aHR0cDovL2Zhdmlj/b25zLnNlYXJjaC5i/cmF2ZS5jb20vaWNv/bnMvZTg1MzQ2NjVk/ZGY5NDczMmYzN2I4/MjYwN2Y4MDdlMDlj/ZjJlZTM3ZjMxOWVl/MTQ0MDFlZDk0MjZh/MWQ3OGU3MC9icmF2/ZS5jb20v",
--           "path": ""
--         },
--         "age": "3 days ago"
--       },
--       {
--         "title": "r/brave on Reddit: Brave search vs Google search",
--         "url": "https://www.reddit.com/r/brave/comments/1an2hb4/brave_search_vs_google_search/",
--         "is_source_local": false,
--         "is_source_both": false,
--         "description": "12 votes, 16 comments. In your view, which option do you find superior for delivering accurate results? I appreciate the summarization feature in…",
--         "page_age": "2024-02-09T23:54:32",
--         "profile": {
--           "name": "Reddit",
--           "url": "https://www.reddit.com/r/brave/comments/1an2hb4/brave_search_vs_google_search/",
--           "long_name": "reddit.com",
--           "img": "https://imgs.search.brave.com/U-eHNCapRHVNWWCVPPMTIvOofZULh0_A_FQKe8xTE4I/rs:fit:32:32:1:0/g:ce/aHR0cDovL2Zhdmlj/b25zLnNlYXJjaC5i/cmF2ZS5jb20vaWNv/bnMvN2ZiNTU0M2Nj/MTFhZjRiYWViZDlk/MjJiMjBjMzFjMDRk/Y2IzYWI0MGI0MjVk/OGY5NzQzOGQ5NzQ5/NWJhMWI0NC93d3cu/cmVkZGl0LmNvbS8"
--         },
--         "language": "en",
--         "family_friendly": true,
--         "type": "search_result",
--         "subtype": "qa",
--         "meta_url": {
--           "scheme": "https",
--           "netloc": "reddit.com",
--           "hostname": "www.reddit.com",
--           "favicon": "https://imgs.search.brave.com/U-eHNCapRHVNWWCVPPMTIvOofZULh0_A_FQKe8xTE4I/rs:fit:32:32:1:0/g:ce/aHR0cDovL2Zhdmlj/b25zLnNlYXJjaC5i/cmF2ZS5jb20vaWNv/bnMvN2ZiNTU0M2Nj/MTFhZjRiYWViZDlk/MjJiMjBjMzFjMDRk/Y2IzYWI0MGI0MjVk/OGY5NzQzOGQ5NzQ5/NWJhMWI0NC93d3cu/cmVkZGl0LmNvbS8",
--           "path": "  › r/brave  › brave search vs google search"
--         },
--         "age": "February 9, 2024"
--       },
--       {
--         "title": "Brave Private Web Browser, VPN - Apps on Google Play",
--         "url": "https://play.google.com/store/apps/details?id=com.brave.browser&hl=en_US",
--         "is_source_local": false,
--         "is_source_both": false,
--         "description": "Get a lightning fast, safe and private web browser with AI, Adblock, and VPN. Loved by 65 million users, <strong>Brave</strong> has AI assistant <strong>Brave</strong> Leo, Firewall + VPN, <strong>Brave</strong> <strong>Search</strong>, and night mode. NEW App Features ✓ <strong>Brave</strong> Leo: AI assistant ✓ Firewall + VPN ✓ <strong>Brave</strong> <strong>Search</strong>.",
--         "page_age": "2024-09-25T19:00:02",
--         "profile": {
--           "name": "Google Play",
--           "url": "https://play.google.com/store/apps/details?id=com.brave.browser&hl=en_US",
--           "long_name": "play.google.com",
--           "img": "https://imgs.search.brave.com/BRXcD4iaX7N7RXX5b2ues7rPiumLO6jABWJKycKQM3o/rs:fit:32:32:1:0/g:ce/aHR0cDovL2Zhdmlj/b25zLnNlYXJjaC5i/cmF2ZS5jb20vaWNv/bnMvYjgwZWUzMzVj/YTc2M2VmMDJlYjE2/YmIyYzA3Y2IwZDkx/ZGViYmUxOGU1Njk4/NGI0NDNhOTQ1N2Mw/MDYyNjk4Ny9wbGF5/Lmdvb2dsZS5jb20v"
--         },
--         "language": "en",
--         "family_friendly": true,
--         "type": "search_result",
--         "subtype": "creative_work",
--         "meta_url": {
--           "scheme": "https",
--           "netloc": "play.google.com",
--           "hostname": "play.google.com",
--           "favicon": "https://imgs.search.brave.com/BRXcD4iaX7N7RXX5b2ues7rPiumLO6jABWJKycKQM3o/rs:fit:32:32:1:0/g:ce/aHR0cDovL2Zhdmlj/b25zLnNlYXJjaC5i/cmF2ZS5jb20vaWNv/bnMvYjgwZWUzMzVj/YTc2M2VmMDJlYjE2/YmIyYzA3Y2IwZDkx/ZGViYmUxOGU1Njk4/NGI0NDNhOTQ1N2Mw/MDYyNjk4Ny9wbGF5/Lmdvb2dsZS5jb20v",
--           "path": "› store  › apps  › details"
--         },
--         "age": "2 weeks ago"
--       },
--       {
--         "title": "r/brave_browser on Reddit: What are your thoughts on Brave Search",
--         "url": "https://www.reddit.com/r/brave_browser/comments/yapl0w/what_are_your_thoughts_on_brave_search/",
--         "is_source_local": false,
--         "is_source_both": false,
--         "description": "38 votes, 52 comments. What are your thoughts with <strong>Brave</strong> <strong>Search</strong> compares to Duckduck <strong>search</strong> or Google <strong>search</strong>",
--         "page_age": "2022-10-22T14:43:52",
--         "profile": {
--           "name": "Reddit",
--           "url": "https://www.reddit.com/r/brave_browser/comments/yapl0w/what_are_your_thoughts_on_brave_search/",
--           "long_name": "reddit.com",
--           "img": "https://imgs.search.brave.com/U-eHNCapRHVNWWCVPPMTIvOofZULh0_A_FQKe8xTE4I/rs:fit:32:32:1:0/g:ce/aHR0cDovL2Zhdmlj/b25zLnNlYXJjaC5i/cmF2ZS5jb20vaWNv/bnMvN2ZiNTU0M2Nj/MTFhZjRiYWViZDlk/MjJiMjBjMzFjMDRk/Y2IzYWI0MGI0MjVk/OGY5NzQzOGQ5NzQ5/NWJhMWI0NC93d3cu/cmVkZGl0LmNvbS8"
--         },
--         "language": "en",
--         "family_friendly": true,
--         "type": "search_result",
--         "subtype": "qa",
--         "meta_url": {
--           "scheme": "https",
--           "netloc": "reddit.com",
--           "hostname": "www.reddit.com",
--           "favicon": "https://imgs.search.brave.com/U-eHNCapRHVNWWCVPPMTIvOofZULh0_A_FQKe8xTE4I/rs:fit:32:32:1:0/g:ce/aHR0cDovL2Zhdmlj/b25zLnNlYXJjaC5i/cmF2ZS5jb20vaWNv/bnMvN2ZiNTU0M2Nj/MTFhZjRiYWViZDlk/MjJiMjBjMzFjMDRk/Y2IzYWI0MGI0MjVk/OGY5NzQzOGQ5NzQ5/NWJhMWI0NC93d3cu/cmVkZGl0LmNvbS8",
--           "path": "  › r/brave_browser  › what are your thoughts on brave search"
--         },
--         "age": "October 22, 2022"
--       },
--       {
--         "title": "r/brave_browser on Reddit: Brave search is so good, and it's my default search engine now!",
--         "url": "https://www.reddit.com/r/brave_browser/comments/uk3xux/brave_search_is_so_good_and_its_my_default_search/",
--         "is_source_local": false,
--         "is_source_both": false,
--         "description": "104 votes, 27 comments. Ok, I am an early user of <strong>brave</strong> <strong>search</strong>, and to be honest, it is not great at the beginning. But recently, when I switch to…",
--         "page_age": "2022-09-16T03:05:01",
--         "profile": {
--           "name": "Reddit",
--           "url": "https://www.reddit.com/r/brave_browser/comments/uk3xux/brave_search_is_so_good_and_its_my_default_search/",
--           "long_name": "reddit.com",
--           "img": "https://imgs.search.brave.com/U-eHNCapRHVNWWCVPPMTIvOofZULh0_A_FQKe8xTE4I/rs:fit:32:32:1:0/g:ce/aHR0cDovL2Zhdmlj/b25zLnNlYXJjaC5i/cmF2ZS5jb20vaWNv/bnMvN2ZiNTU0M2Nj/MTFhZjRiYWViZDlk/MjJiMjBjMzFjMDRk/Y2IzYWI0MGI0MjVk/OGY5NzQzOGQ5NzQ5/NWJhMWI0NC93d3cu/cmVkZGl0LmNvbS8"
--         },
--         "language": "en",
--         "family_friendly": true,
--         "type": "search_result",
--         "subtype": "qa",
--         "meta_url": {
--           "scheme": "https",
--           "netloc": "reddit.com",
--           "hostname": "www.reddit.com",
--           "favicon": "https://imgs.search.brave.com/U-eHNCapRHVNWWCVPPMTIvOofZULh0_A_FQKe8xTE4I/rs:fit:32:32:1:0/g:ce/aHR0cDovL2Zhdmlj/b25zLnNlYXJjaC5i/cmF2ZS5jb20vaWNv/bnMvN2ZiNTU0M2Nj/MTFhZjRiYWViZDlk/MjJiMjBjMzFjMDRk/Y2IzYWI0MGI0MjVk/OGY5NzQzOGQ5NzQ5/NWJhMWI0NC93d3cu/cmVkZGl0LmNvbS8",
--           "path": "  › r/brave_browser  › brave search is so good, and it's my default search engine now!"
--         },
--         "age": "September 16, 2022"
--       },
--       {
--         "title": "Brave Search is NOT good - Brave Search - Brave Community",
--         "url": "https://community.brave.com/t/brave-search-is-not-good/490801",
--         "is_source_local": false,
--         "is_source_both": false,
--         "description": "Hi guys, A Firefox user for many, many years, I switched over to <strong>Brave</strong> probably around 2 - 3 years ago. I like many features of the browser. But there is one huge glaring problem…the <strong>search</strong> engine. Not to put too fine a point on it, but it’s lame, as in it does not return relevant results ...",
--         "page_age": "2023-06-06T15:02:18",
--         "profile": {
--           "name": "Brave Community",
--           "url": "https://community.brave.com/t/brave-search-is-not-good/490801",
--           "long_name": "community.brave.com",
--           "img": "https://imgs.search.brave.com/0CmlJilpWBlAXujGXMFBWZJEZytA2l9Zv5NMmfMcK0I/rs:fit:32:32:1:0/g:ce/aHR0cDovL2Zhdmlj/b25zLnNlYXJjaC5i/cmF2ZS5jb20vaWNv/bnMvZGUwZmE0ZGUy/MzZmOWJjYzgzMjE4/NWM5NmE4NzBjMjFl/YzcwZDY4YjY3ZmVm/YzRiNzIxZDgwZDhh/YzliODRkYy9jb21t/dW5pdHkuYnJhdmUu/Y29tLw"
--         },
--         "language": "en",
--         "family_friendly": true,
--         "type": "search_result",
--         "subtype": "qa",
--         "meta_url": {
--           "scheme": "https",
--           "netloc": "community.brave.com",
--           "hostname": "community.brave.com",
--           "favicon": "https://imgs.search.brave.com/0CmlJilpWBlAXujGXMFBWZJEZytA2l9Zv5NMmfMcK0I/rs:fit:32:32:1:0/g:ce/aHR0cDovL2Zhdmlj/b25zLnNlYXJjaC5i/cmF2ZS5jb20vaWNv/bnMvZGUwZmE0ZGUy/MzZmOWJjYzgzMjE4/NWM5NmE4NzBjMjFl/YzcwZDY4YjY3ZmVm/YzRiNzIxZDgwZDhh/YzliODRkYy9jb21t/dW5pdHkuYnJhdmUu/Y29tLw",
--           "path": "  › brave search"
--         },
--         "age": "June 6, 2023"
--       },
--       {
--         "title": "‎Brave Private Web Browser, VPN on the App Store",
--         "url": "https://apps.apple.com/us/app/brave-private-web-browser-vpn/id1052879175",
--         "is_source_local": false,
--         "is_source_both": false,
--         "description": "‎Browse 3X faster on <strong>Brave</strong> Browser, now with AI. You’ll get a fast, safe, private web browser with Adblock and VPN. Loved by 60 million users, <strong>Brave</strong> has an AI assistant <strong>Brave</strong> Leo, Firewall + VPN, <strong>Brave</strong> Wallet, <strong>Brave</strong> <strong>Search</strong>, <strong>Brave</strong> Playlist, and night mode. NEW Features: • <strong>Brave</strong> Leo: AI ...",
--         "page_age": "2024-08-17T00:00:00",
--         "profile": {
--           "name": "App Store",
--           "url": "https://apps.apple.com/us/app/brave-private-web-browser-vpn/id1052879175",
--           "long_name": "apps.apple.com",
--           "img": "https://imgs.search.brave.com/_z2UHgNWh4jJ6brYGbXxpDTMJmD9b1fxdUBJ2mJOP4w/rs:fit:32:32:1:0/g:ce/aHR0cDovL2Zhdmlj/b25zLnNlYXJjaC5i/cmF2ZS5jb20vaWNv/bnMvMDYzNzRkMjA3/NzAwOWZhNjI1MmM5/NzRiMzIwZGQ4YzZl/ZDgwYmM0Zjk3MDE2/ZDM1Y2FlMDBiNjVi/OWEzZDIzNy9hcHBz/LmFwcGxlLmNvbS8"
--         },
--         "language": "en",
--         "family_friendly": true,
--         "type": "search_result",
--         "subtype": "creative_work",
--         "meta_url": {
--           "scheme": "https",
--           "netloc": "apps.apple.com",
--           "hostname": "apps.apple.com",
--           "favicon": "https://imgs.search.brave.com/_z2UHgNWh4jJ6brYGbXxpDTMJmD9b1fxdUBJ2mJOP4w/rs:fit:32:32:1:0/g:ce/aHR0cDovL2Zhdmlj/b25zLnNlYXJjaC5i/cmF2ZS5jb20vaWNv/bnMvMDYzNzRkMjA3/NzAwOWZhNjI1MmM5/NzRiMzIwZGQ4YzZl/ZDgwYmM0Zjk3MDE2/ZDM1Y2FlMDBiNjVi/OWEzZDIzNy9hcHBz/LmFwcGxlLmNvbS8",
--           "path": "› us  › app  › brave-private-web-browser-vpn  › id1052879175"
--         },
--         "age": "August 17, 2024"
--       },
--       {
--         "title": "Brave (@brave) · X",
--         "url": "https://twitter.com/brave",
--         "is_source_local": false,
--         "is_source_both": false,
--         "description": "The latest tweets from <strong>Brave</strong> (@<strong>brave</strong>)",
--         "page_age": "2022-09-28T00:00:00",
--         "profile": {
--           "name": "X",
--           "url": "https://twitter.com/brave",
--           "long_name": "twitter.com",
--           "img": "https://imgs.search.brave.com/wWeP6Pa-JGi7cDBqMPkc4Ckai-nUQl4uFSKrw0evyx4/rs:fit:32:32:1:0/g:ce/aHR0cDovL2Zhdmlj/b25zLnNlYXJjaC5i/cmF2ZS5jb20vaWNv/bnMvN2MxOTUxNzhj/OTY1ZTQ3N2I0MjJk/MTY5NGM0MTRlYWVi/MjU1YWE2NDUwYmQ2/YTA2MDFhMDlkZDEx/NTAzZGNiNi90d2l0/dGVyLmNvbS8"
--         },
--         "language": "en",
--         "family_friendly": true,
--         "type": "search_result",
--         "subtype": "generic",
--         "meta_url": {
--           "scheme": "https",
--           "netloc": "twitter.com",
--           "hostname": "twitter.com",
--           "favicon": "https://imgs.search.brave.com/wWeP6Pa-JGi7cDBqMPkc4Ckai-nUQl4uFSKrw0evyx4/rs:fit:32:32:1:0/g:ce/aHR0cDovL2Zhdmlj/b25zLnNlYXJjaC5i/cmF2ZS5jb20vaWNv/bnMvN2MxOTUxNzhj/OTY1ZTQ3N2I0MjJk/MTY5NGM0MTRlYWVi/MjU1YWE2NDUwYmQ2/YTA2MDFhMDlkZDEx/NTAzZGNiNi90d2l0/dGVyLmNvbS8",
--           "path": "› brave"
--         },
--         "age": "September 28, 2022"
--       },
--       {
--         "title": "r/brave_browser on Reddit: How bad is it if I use Google as my search engine in my Brave browser?",
--         "url": "https://www.reddit.com/r/brave_browser/comments/18yswy8/how_bad_is_it_if_i_use_google_as_my_search_engine/",
--         "is_source_local": false,
--         "is_source_both": false,
--         "description": "30 votes, 74 comments. Hi guys, Yesterday switched to <strong>Brave</strong> and I&#x27;m very happy so far, but I&#x27;d like to use Google as my <strong>search</strong> engine. Can you please…",
--         "page_age": "2023-12-31T15:15:31",
--         "profile": {
--           "name": "Reddit",
--           "url": "https://www.reddit.com/r/brave_browser/comments/18yswy8/how_bad_is_it_if_i_use_google_as_my_search_engine/",
--           "long_name": "reddit.com",
--           "img": "https://imgs.search.brave.com/U-eHNCapRHVNWWCVPPMTIvOofZULh0_A_FQKe8xTE4I/rs:fit:32:32:1:0/g:ce/aHR0cDovL2Zhdmlj/b25zLnNlYXJjaC5i/cmF2ZS5jb20vaWNv/bnMvN2ZiNTU0M2Nj/MTFhZjRiYWViZDlk/MjJiMjBjMzFjMDRk/Y2IzYWI0MGI0MjVk/OGY5NzQzOGQ5NzQ5/NWJhMWI0NC93d3cu/cmVkZGl0LmNvbS8"
--         },
--         "language": "en",
--         "family_friendly": true,
--         "type": "search_result",
--         "subtype": "qa",
--         "meta_url": {
--           "scheme": "https",
--           "netloc": "reddit.com",
--           "hostname": "www.reddit.com",
--           "favicon": "https://imgs.search.brave.com/U-eHNCapRHVNWWCVPPMTIvOofZULh0_A_FQKe8xTE4I/rs:fit:32:32:1:0/g:ce/aHR0cDovL2Zhdmlj/b25zLnNlYXJjaC5i/cmF2ZS5jb20vaWNv/bnMvN2ZiNTU0M2Nj/MTFhZjRiYWViZDlk/MjJiMjBjMzFjMDRk/Y2IzYWI0MGI0MjVk/OGY5NzQzOGQ5NzQ5/NWJhMWI0NC93d3cu/cmVkZGl0LmNvbS8",
--           "path": "  › r/brave_browser  › how bad is it if i use google as my search engine in my brave browser?"
--         },
--         "age": "December 31, 2023"
--       },
--       {
--         "title": "Brave Browser Download | Brave",
--         "url": "https://brave.com/download/",
--         "is_source_local": false,
--         "is_source_both": false,
--         "description": "The latest version of the <strong>Brave</strong> browser with ad and tracker blocking capabilities is available to download here. Stand by for an entirely new way of thinking about how the web can work.",
--         "page_age": "2023-08-06T00:00:00",
--         "profile": {
--           "name": "Brave",
--           "url": "https://brave.com/download/",
--           "long_name": "brave.com",
--           "img": "https://imgs.search.brave.com/vnilI-MeUh5_v9ywDuoJrW0Q4m2J3p50IzXyFgVktSA/rs:fit:32:32:1:0/g:ce/aHR0cDovL2Zhdmlj/b25zLnNlYXJjaC5i/cmF2ZS5jb20vaWNv/bnMvZTg1MzQ2NjVk/ZGY5NDczMmYzN2I4/MjYwN2Y4MDdlMDlj/ZjJlZTM3ZjMxOWVl/MTQ0MDFlZDk0MjZh/MWQ3OGU3MC9icmF2/ZS5jb20v"
--         },
--         "language": "en",
--         "family_friendly": true,
--         "type": "search_result",
--         "subtype": "generic",
--         "meta_url": {
--           "scheme": "https",
--           "netloc": "brave.com",
--           "hostname": "brave.com",
--           "favicon": "https://imgs.search.brave.com/vnilI-MeUh5_v9ywDuoJrW0Q4m2J3p50IzXyFgVktSA/rs:fit:32:32:1:0/g:ce/aHR0cDovL2Zhdmlj/b25zLnNlYXJjaC5i/cmF2ZS5jb20vaWNv/bnMvZTg1MzQ2NjVk/ZGY5NDczMmYzN2I4/MjYwN2Y4MDdlMDlj/ZjJlZTM3ZjMxOWVl/MTQ0MDFlZDk0MjZh/MWQ3OGU3MC9icmF2/ZS5jb20v",
--           "path": "› download"
--         },
--         "age": "August 6, 2023"
--       },
--       {
--         "title": "Brave Search news | Brave",
--         "url": "https://brave.com/category/brave-search-news/",
--         "is_source_local": false,
--         "is_source_both": false,
--         "description": "The <strong>Brave</strong> browser is a fast, private and secure web browser for PC, Mac and mobile. Download now to enjoy a faster ad-free browsing experience that saves data and battery life by blocking tracking software.",
--         "page_age": "2024-03-27T00:00:00",
--         "profile": {
--           "name": "Brave",
--           "url": "https://brave.com/category/brave-search-news/",
--           "long_name": "brave.com",
--           "img": "https://imgs.search.brave.com/vnilI-MeUh5_v9ywDuoJrW0Q4m2J3p50IzXyFgVktSA/rs:fit:32:32:1:0/g:ce/aHR0cDovL2Zhdmlj/b25zLnNlYXJjaC5i/cmF2ZS5jb20vaWNv/bnMvZTg1MzQ2NjVk/ZGY5NDczMmYzN2I4/MjYwN2Y4MDdlMDlj/ZjJlZTM3ZjMxOWVl/MTQ0MDFlZDk0MjZh/MWQ3OGU3MC9icmF2/ZS5jb20v"
--         },
--         "language": "en",
--         "family_friendly": true,
--         "type": "search_result",
--         "subtype": "generic",
--         "meta_url": {
--           "scheme": "https",
--           "netloc": "brave.com",
--           "hostname": "brave.com",
--           "favicon": "https://imgs.search.brave.com/vnilI-MeUh5_v9ywDuoJrW0Q4m2J3p50IzXyFgVktSA/rs:fit:32:32:1:0/g:ce/aHR0cDovL2Zhdmlj/b25zLnNlYXJjaC5i/cmF2ZS5jb20vaWNv/bnMvZTg1MzQ2NjVk/ZGY5NDczMmYzN2I4/MjYwN2Y4MDdlMDlj/ZjJlZTM3ZjMxOWVl/MTQ0MDFlZDk0MjZh/MWQ3OGU3MC9icmF2/ZS5jb20v",
--           "path": "› category  › brave-search-news"
--         },
--         "age": "March 27, 2024"
--       },
--       {
--         "title": "Brave's Search Engine & Search index - Brave Community",
--         "url": "https://community.brave.com/t/braves-search-engine-search-index/490420",
--         "is_source_local": false,
--         "is_source_both": false,
--         "description": "Is there something on the pipeline where all image <strong>search</strong> or video <strong>search</strong> are independently indexed? Why couldn’t y’all just proxy in the direct sense like you did before? I would like to index my website/landing page as well as anything relating to business on <strong>Brave</strong> <strong>Search</strong> Index. How do ...",
--         "page_age": "2023-06-03T23:10:42",
--         "profile": {
--           "name": "Brave Community",
--           "url": "https://community.brave.com/t/braves-search-engine-search-index/490420",
--           "long_name": "community.brave.com",
--           "img": "https://imgs.search.brave.com/0CmlJilpWBlAXujGXMFBWZJEZytA2l9Zv5NMmfMcK0I/rs:fit:32:32:1:0/g:ce/aHR0cDovL2Zhdmlj/b25zLnNlYXJjaC5i/cmF2ZS5jb20vaWNv/bnMvZGUwZmE0ZGUy/MzZmOWJjYzgzMjE4/NWM5NmE4NzBjMjFl/YzcwZDY4YjY3ZmVm/YzRiNzIxZDgwZDhh/YzliODRkYy9jb21t/dW5pdHkuYnJhdmUu/Y29tLw"
--         },
--         "language": "en",
--         "family_friendly": true,
--         "type": "search_result",
--         "subtype": "article",
--         "meta_url": {
--           "scheme": "https",
--           "netloc": "community.brave.com",
--           "hostname": "community.brave.com",
--           "favicon": "https://imgs.search.brave.com/0CmlJilpWBlAXujGXMFBWZJEZytA2l9Zv5NMmfMcK0I/rs:fit:32:32:1:0/g:ce/aHR0cDovL2Zhdmlj/b25zLnNlYXJjaC5i/cmF2ZS5jb20vaWNv/bnMvZGUwZmE0ZGUy/MzZmOWJjYzgzMjE4/NWM5NmE4NzBjMjFl/YzcwZDY4YjY3ZmVm/YzRiNzIxZDgwZDhh/YzliODRkYy9jb21t/dW5pdHkuYnJhdmUu/Y29tLw",
--           "path": "› t  › braves-search-engine-search-index  › 490420"
--         },
--         "age": "June 3, 2023"
--       },
--       {
--         "title": "Searching results are so bad - Search Feedback - Brave Community",
--         "url": "https://community.brave.com/t/searching-results-are-so-bad/429001",
--         "is_source_local": false,
--         "is_source_both": false,
--         "description": "I find myself on multiple occasion just switching back to a different <strong>search</strong> engine because the results vary so randomly.",
--         "page_age": "2022-09-06T00:00:00",
--         "profile": {
--           "name": "Brave Community",
--           "url": "https://community.brave.com/t/searching-results-are-so-bad/429001",
--           "long_name": "community.brave.com",
--           "img": "https://imgs.search.brave.com/0CmlJilpWBlAXujGXMFBWZJEZytA2l9Zv5NMmfMcK0I/rs:fit:32:32:1:0/g:ce/aHR0cDovL2Zhdmlj/b25zLnNlYXJjaC5i/cmF2ZS5jb20vaWNv/bnMvZGUwZmE0ZGUy/MzZmOWJjYzgzMjE4/NWM5NmE4NzBjMjFl/YzcwZDY4YjY3ZmVm/YzRiNzIxZDgwZDhh/YzliODRkYy9jb21t/dW5pdHkuYnJhdmUu/Y29tLw"
--         },
--         "language": "en",
--         "family_friendly": true,
--         "type": "search_result",
--         "subtype": "article",
--         "meta_url": {
--           "scheme": "https",
--           "netloc": "community.brave.com",
--           "hostname": "community.brave.com",
--           "favicon": "https://imgs.search.brave.com/0CmlJilpWBlAXujGXMFBWZJEZytA2l9Zv5NMmfMcK0I/rs:fit:32:32:1:0/g:ce/aHR0cDovL2Zhdmlj/b25zLnNlYXJjaC5i/cmF2ZS5jb20vaWNv/bnMvZGUwZmE0ZGUy/MzZmOWJjYzgzMjE4/NWM5NmE4NzBjMjFl/YzcwZDY4YjY3ZmVm/YzRiNzIxZDgwZDhh/YzliODRkYy9jb21t/dW5pdHkuYnJhdmUu/Y29tLw",
--           "path": "  › brave search  › search feedback"
--         },
--         "age": "September 6, 2022"
--       },
--       {
--         "title": "Google gets a new rival as Brave Search opens to the public - CNET",
--         "url": "https://www.cnet.com/tech/mobile/google-gets-a-new-rival-as-brave-search-opens-to-the-public/",
--         "is_source_local": false,
--         "is_source_both": false,
--         "description": "About 32 million people now use <strong>Brave</strong>&#x27;s ad-blocking browser each month.",
--         "page_age": "2021-07-22T00:00:00",
--         "profile": {
--           "name": "CNET",
--           "url": "https://www.cnet.com/tech/mobile/google-gets-a-new-rival-as-brave-search-opens-to-the-public/",
--           "long_name": "cnet.com",
--           "img": "https://imgs.search.brave.com/mWlG-6ikaw-opvXdUS2O5Ia2lYTwH6MNnfeV64-Abho/rs:fit:32:32:1:0/g:ce/aHR0cDovL2Zhdmlj/b25zLnNlYXJjaC5i/cmF2ZS5jb20vaWNv/bnMvYWVkNmIyYmM4/MmJlODJlZDJjYTBh/MWE2MmMwMmVkOTdk/OThhMmU4ZjE0MTY4/MzAxYTcyNjI5OGI5/OGJhN2NmYS93d3cu/Y25ldC5jb20v"
--         },
--         "language": "en",
--         "family_friendly": true,
--         "type": "search_result",
--         "subtype": "article",
--         "meta_url": {
--           "scheme": "https",
--           "netloc": "cnet.com",
--           "hostname": "www.cnet.com",
--           "favicon": "https://imgs.search.brave.com/mWlG-6ikaw-opvXdUS2O5Ia2lYTwH6MNnfeV64-Abho/rs:fit:32:32:1:0/g:ce/aHR0cDovL2Zhdmlj/b25zLnNlYXJjaC5i/cmF2ZS5jb20vaWNv/bnMvYWVkNmIyYmM4/MmJlODJlZDJjYTBh/MWE2MmMwMmVkOTdk/OThhMmU4ZjE0MTY4/MzAxYTcyNjI5OGI5/OGJhN2NmYS93d3cu/Y25ldC5jb20v",
--           "path": "  › tech  › mobile  › google gets a new rival as brave search opens to the public"
--         },
--         "age": "July 22, 2021"
--       },
--       {
--         "title": "r/ArcBrowser on Reddit: Add support for Brave search?",
--         "url": "https://www.reddit.com/r/ArcBrowser/comments/135q8wl/add_support_for_brave_search/",
--         "is_source_local": false,
--         "is_source_both": false,
--         "description": "I&#x27;ve noticed that Arc, doesn&#x27;t currently support <strong>Brave</strong> <strong>Search</strong> as a <strong>search</strong> engine option. https://<strong>search</strong>.<strong>brave</strong>.com/ I think it would be really neat if…",
--         "page_age": "2023-05-02T15:34:26",
--         "profile": {
--           "name": "Reddit",
--           "url": "https://www.reddit.com/r/ArcBrowser/comments/135q8wl/add_support_for_brave_search/",
--           "long_name": "reddit.com",
--           "img": "https://imgs.search.brave.com/U-eHNCapRHVNWWCVPPMTIvOofZULh0_A_FQKe8xTE4I/rs:fit:32:32:1:0/g:ce/aHR0cDovL2Zhdmlj/b25zLnNlYXJjaC5i/cmF2ZS5jb20vaWNv/bnMvN2ZiNTU0M2Nj/MTFhZjRiYWViZDlk/MjJiMjBjMzFjMDRk/Y2IzYWI0MGI0MjVk/OGY5NzQzOGQ5NzQ5/NWJhMWI0NC93d3cu/cmVkZGl0LmNvbS8"
--         },
--         "language": "en",
--         "family_friendly": true,
--         "type": "search_result",
--         "subtype": "qa",
--         "meta_url": {
--           "scheme": "https",
--           "netloc": "reddit.com",
--           "hostname": "www.reddit.com",
--           "favicon": "https://imgs.search.brave.com/U-eHNCapRHVNWWCVPPMTIvOofZULh0_A_FQKe8xTE4I/rs:fit:32:32:1:0/g:ce/aHR0cDovL2Zhdmlj/b25zLnNlYXJjaC5i/cmF2ZS5jb20vaWNv/bnMvN2ZiNTU0M2Nj/MTFhZjRiYWViZDlk/MjJiMjBjMzFjMDRk/Y2IzYWI0MGI0MjVk/OGY5NzQzOGQ5NzQ5/NWJhMWI0NC93d3cu/cmVkZGl0LmNvbS8",
--           "path": "  › r/arcbrowser  › add support for brave search?"
--         },
--         "age": "May 2, 2023"
--       },
--       {
--         "title": "Brave Search | 🦜️🔗 LangChain",
--         "url": "https://python.langchain.com/docs/integrations/providers/brave_search/",
--         "is_source_local": false,
--         "is_source_both": false,
--         "description": "<strong>Brave</strong> <strong>Search</strong> is a <strong>search</strong> engine developed by <strong>Brave</strong> Software.",
--         "profile": {
--           "name": "Langchain",
--           "url": "https://python.langchain.com/docs/integrations/providers/brave_search/",
--           "long_name": "python.langchain.com",
--           "img": "https://imgs.search.brave.com/HbIQOpVB-LnPWm8VCB_OXwbYJX2_Ifdjd1yCZ2KU7FE/rs:fit:32:32:1:0/g:ce/aHR0cDovL2Zhdmlj/b25zLnNlYXJjaC5i/cmF2ZS5jb20vaWNv/bnMvMTZjYjU5OGVl/MmJhOGQxM2IwZDY2/MzkzNzIzMDY1ZmEw/MzQ3NTZhMDhhODY4/MTM3MjRmMWU1YTU0/OWUwN2I4OS9weXRo/b24ubGFuZ2NoYWlu/LmNvbS8"
--         },
--         "language": "en",
--         "family_friendly": true,
--         "type": "search_result",
--         "subtype": "generic",
--         "meta_url": {
--           "scheme": "https",
--           "netloc": "python.langchain.com",
--           "hostname": "python.langchain.com",
--           "favicon": "https://imgs.search.brave.com/HbIQOpVB-LnPWm8VCB_OXwbYJX2_Ifdjd1yCZ2KU7FE/rs:fit:32:32:1:0/g:ce/aHR0cDovL2Zhdmlj/b25zLnNlYXJjaC5i/cmF2ZS5jb20vaWNv/bnMvMTZjYjU5OGVl/MmJhOGQxM2IwZDY2/MzkzNzIzMDY1ZmEw/MzQ3NTZhMDhhODY4/MTM3MjRmMWU1YTU0/OWUwN2I4OS9weXRo/b24ubGFuZ2NoYWlu/LmNvbS8",
--           "path": "  › providers  › more  › brave search"
--         }
--       },
--       {
--         "title": "The Brave Search Engine. Will This Be The Google Killer?",
--         "url": "https://www.youtube.com/watch?v=lLcwGDQPZ-s",
--         "is_source_local": false,
--         "is_source_both": false,
--         "description": "Enjoy the videos and music you love, upload original content, and share it all with friends, family, and the world on YouTube.",
--         "page_age": "2021-06-23T00:00:00",
--         "profile": {
--           "name": "YouTube",
--           "url": "https://www.youtube.com/watch?v=lLcwGDQPZ-s",
--           "long_name": "youtube.com",
--           "img": "https://imgs.search.brave.com/Wg4wjE5SHAargkzePU3eSLmWgVz84BEZk1SjSglJK_U/rs:fit:32:32:1:0/g:ce/aHR0cDovL2Zhdmlj/b25zLnNlYXJjaC5i/cmF2ZS5jb20vaWNv/bnMvOTkyZTZiMWU3/YzU3Nzc5YjExYzUy/N2VhZTIxOWNlYjM5/ZGVjN2MyZDY4Nzdh/ZDYzMTYxNmI5N2Rk/Y2Q3N2FkNy93d3cu/eW91dHViZS5jb20v"
--         },
--         "language": "en",
--         "family_friendly": true,
--         "type": "search_result",
--         "subtype": "generic",
--         "meta_url": {
--           "scheme": "https",
--           "netloc": "youtube.com",
--           "hostname": "www.youtube.com",
--           "favicon": "https://imgs.search.brave.com/Wg4wjE5SHAargkzePU3eSLmWgVz84BEZk1SjSglJK_U/rs:fit:32:32:1:0/g:ce/aHR0cDovL2Zhdmlj/b25zLnNlYXJjaC5i/cmF2ZS5jb20vaWNv/bnMvOTkyZTZiMWU3/YzU3Nzc5YjExYzUy/N2VhZTIxOWNlYjM5/ZGVjN2MyZDY4Nzdh/ZDYzMTYxNmI5N2Rk/Y2Q3N2FkNy93d3cu/eW91dHViZS5jb20v",
--           "path": "› watch"
--         },
--         "age": "June 23, 2021"
--       }
--     ],
--     "family_friendly": true
--   }
-- }
--
-- Given a JSON (using aeson) payload like the above, we can parse it into a Haskell data structure like this:

data WebSearchApiResponse = WebSearchApiResponse
    { web :: Search.Search
    , query :: Query.Query
    --   , summarizer :: Maybe Summarizer
    }
    deriving (Show, Generic, FromJSON, ToJSON)

-- data Profile = Profile
--   { name :: Text
--   -- , url :: Text
--   , longName :: Text
--   , img :: Text
--   }
--   deriving (Show, Generic, FromJSON)
--
-- data MetaUrl = MetaUrl
--   { scheme :: Text
--   , netloc :: Text
--   , hostname :: Text
--   , favicon :: Text
--   , path :: Text
--   } deriving (Show, Generic, FromJSON)

-- The above data types are derived from the JSON payload. The `FromJSON` typeclass is used to parse the JSON payload into the Haskell data structure.
