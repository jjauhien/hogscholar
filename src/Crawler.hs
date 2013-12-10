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
{-# LANGUAGE OverloadedStrings #-}

module Crawler where

import Data.List
import Data.Maybe

import Network.CGI.Protocol
import Network.HTTP
import Network.URI

-- import Network.HTTP.Conduit (simpleHttp)
import qualified Data.Text as T
import Text.HTML.DOM (parseLBS)
import Text.XML.Cursor (Cursor, attributeIs, content, element, fromDocument, child,
                        ($//), (&|), (&//), (>=>))

import Data.ByteString.Lazy.Char8 (pack)


import Papers (Author(Author), Paper, PaperGraph)

data Query = Query {
      qAuthors :: [Author]
    , qTitle   :: Maybe String
    , qJournal :: Maybe String
    , qYear    :: Maybe Int
    }

-- downloads paper and the papers which refer it (not more than n)
downloadPaper :: Query -> IO (Maybe (Paper,[Paper]))
downloadPaper q = undefined


--downloadPaper' :: Query -> IO (Maybe (Paper, String))


-- parses all the papers on the results page
-- returns a list (Paper, Request to get list of citators)
-- parsePage :: String -> [(Paper, Request String)]
parsePage = map node2paper . ($// findPaperNodes) . fromDocument . parseLBS . pack


-- all div elements, which have class "gs_r"
findPaperNodes :: Cursor -> [Cursor]
findPaperNodes = element "div" >=> attributeIs "class" "gs_r"


-- parses one node which represents one paper
--node2paper :: Cursor -> (Paper, Request String)
node2paper cur = caption
    where
    caption =  {-T.concat . content . -}head .
        ($// (element "h3" >=> attributeIs "class" "gs_rt")) $ cur

findCaption = (element "h3" >=> attributeIs "class" "gs_rt")

-- converts tag tree to simple text
pruneTags node = undefined


downloadHTML :: Request String -> IO (Maybe String)
downloadHTML link = do
    resp <- simpleHTTP link
    case resp of
        Left _  -> return Nothing   -- some error
        Right r -> case rspCode r of
                    (2,_,_) -> return $ Just (rspBody r)
                    _       -> return Nothing  -- e.g. redirect FIXME


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

onlyK :: String -> Maybe (String, String)
onlyK key = formKV key (Just "")


dummyRequest = formRequest $
    Query [Author "Liu", Author "Lin", Author "Lin", Author "Chung"]
        (Just "A 480mb/s LDPC-COFDM-based UWB baseband transceiver")
        (Just "Digest of Technical Papers")
        (Just 2005)

dummyPage = downloadHTML dummyRequest

crawlPapers :: Query -> IO (Maybe PaperGraph)
crawlPapers = undefined

{- TODO:
    form HTTP query for Google Scholar
    receive the resulting HTML page
    parse it to find the 1st paper and cites list
    return it
    crawling: internal limit on papers. Emm... somehow
-}
