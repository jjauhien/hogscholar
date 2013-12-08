-----------------------------------------------------------------------------
--
-- Module      :  Crawler
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Crawler where

import Data.List
import Data.Maybe

import Network.CGI.Protocol
import Network.HTTP
import Network.URI


import Papers

data Query = Query {
      qAuthors :: [Author]
    , qTitle   :: Maybe String
    , qJournal :: Maybe String
    , qYear    :: Maybe Int
    }

-- downloads paper and the papers which refer it (not more than n)
downloadPaper :: Query -> Int -> IO (Maybe (Paper,[Paper]))
downloadPaper q n = undefined

downloadHTML :: Request String -> IO (Maybe String)
downloadHTML link = do
    resp <- simpleHTTP link
    case resp of
        Left _  -> return Nothing   -- some error
        Right r -> case rspCode r of
                    (2,_,_) -> return $ Just (rspBody r)
                    _       -> return Nothing


-- creates query string
formRequest :: Query -> Request String
formRequest (Query authors title journal year) =
        Request { rqURI     = uri
                , rqMethod  = GET
                , rqHeaders = []
                , rqBody    = ""}
    where
    uri = URI { uriScheme    = "http:"
              , uriAuthority = Just uriAuth
              , uriPath      = "/scholar"
              , uriQuery     = '?' : uriParams
              , uriFragment  = ""}
    uriAuth = URIAuth { uriUserInfo = ""
                      , uriRegName  = "scholar.google.com"
                      , uriPort     = ""}
    uriParams = formEncode $ catMaybes
                    [ formKV "as_q" title
                    , onlyK "as_epq"
                    , onlyK "as_oq"
                    , onlyK "as_eq"
                    , formKV "as_occt" (Just "title")
                    , formKV "as_sauthors" (Just . intercalate ", " $ map (\(Author a) -> a) authors)
                    , formKV "as_publication" journal
                    , formKV "as_ylo" (show `fmap` year)
                    , formKV "as_yhi" (show `fmap` year)
                    , onlyK "btnG"
                    , formKV "hl" (Just "en")
                    , formKV "as_sdt" (Just "0,5")
                    ]


formKV :: String -> Maybe String -> Maybe (String,String)
formKV key val = (\v -> (key,v)) `fmap` val

onlyK key = formKV key (Just "")

dummyRequest = formRequest $
    Query [Author "Liu", Author "Lin", Author "Lin", Author "Chung"]
        (Just "A 480mb/s LDPC-COFDM-based UWB baseband transceiver")
        (Just "Digest of Technical Papers")
        (Just 2005)


crawlPapers :: Query -> IO (Maybe PaperGraph)
crawlPapers = undefined

{- TODO:
    form HTTP query for Google Scholar
    receive the resulting HTML page
    parse it to find the 1st paper and cites list
    return it
    crawling: internal limit on papers. Emm... somehow
-}
