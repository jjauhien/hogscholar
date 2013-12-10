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

import Data.ByteString.Lazy.Char8 (pack)
import Data.List
import Data.List.Split (wordsBy, splitOn)
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Text as T

import Control.Monad (guard)
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class (lift)

import Network.CGI.Protocol
import Network.HTTP
import Network.URI

import Text.HTML.DOM (parseLBS)
import Text.XML (Node(NodeElement,NodeContent), elementNodes, elementAttributes,
                 nameLocalName)
import Text.XML.Cursor (Cursor, Axis, attributeIs, content, element,
                        child, node, fromDocument,
                        ($//), (&|), (&//), (>=>))

import Papers (Author(Author), Paper(Paper), PaperGraph)

-----------------------------------------------------------------------------

data Query = Query {
      qAuthors :: [Author]
    , qTitle   :: Maybe String
    , qJournal :: Maybe String
    , qYear    :: Maybe Int
    }

-----------------------------------------------------------------------------
-- Networking
-----------------------------------------------------------------------------

-- downloads paper and the papers which refer it (not more than n)
-- FIXME: use MaybeT
downloadPaper :: Query -> IO (Maybe (Paper,[Paper])) -- MaybeT IO (Paper,[Paper])
downloadPaper q = do
    s <- downloadHTML . formRequest $ q
    case s >>= listToMaybe . parsePage of
        Nothing -> return Nothing
        Just (pap, req) -> do
            resp <- downloadHTML req
            case resp of
                Nothing -> return $ Just (pap, [])
                Just s1 -> return $ Just (pap, map fst . parsePage $ s1)
    where
    getCites (pap, req) = undefined
    mapSnd f (x,y) = (x,f y)


downloadHTML :: Request String -> IO (Maybe String)
downloadHTML link = do
    resp <- simpleHTTP link
    case resp of
        Left _  -> return Nothing   -- some error
        Right r -> case rspCode r of
                    (2,_,_) -> return $ Just (rspBody r)
                    _       -> return Nothing  -- e.g. redirect FIXME


-- creates request
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
    formKV key val = (\v -> (key,v)) `fmap` val
    onlyK  key     = formKV key (Just "")


dummyQuery = Query [Author "Liu", Author "Lin", Author "Lin", Author "Chung"]
                    (Just "A 480mb/s LDPC-COFDM-based UWB baseband transceiver")
                    (Just "Digest of Technical Papers")
                    (Just 2005)
dummyRequest = formRequest dummyQuery
dummyPage = downloadHTML dummyRequest


crawlPapers :: Query -> IO (Maybe PaperGraph)
crawlPapers = undefined

-----------------------------------------------------------------------------
-- HTML/XML parsing
-----------------------------------------------------------------------------

-- parses all the papers on the results page
-- returns a list (Paper, Request to get list of citators)
parsePage :: String -> [(Paper, Request String)]
parsePage = map node2paper . ($// findPaperNodes) . fromDocument . parseLBS . pack


-- all div elements, which have class "gs_r"
findPaperNodes :: Axis
findPaperNodes = element "div" >=> attributeIs "class" "gs_r"


-- parses one node which represents one paper
node2paper :: Cursor -> (Paper, Request String)
node2paper cur = (Paper auths titl jour yr,
                  Request {rqURI = fromJust $ parseURI cites
                         , rqMethod = GET
                         , rqHeaders = []
                         , rqBody = ""
                         })
    where
    titl = pruneSpaces . dropFileType . pruneTags . head .
        ($// (element "h3" >=> attributeIs "class" "gs_rt")) $ cur
    dropFileType str = if str' == str then str else dropFileType str'
        where helper str = if elemIndex '[' str == Just 0
                            then
                                case elemIndex ']' str of
                                    Nothing -> str
                                    Just i  -> drop (i+1) str
                            else str
              str' = helper str
    authJourYr = splitOn " - " . filter (/= '\8230') .pruneTags . head .
        ($// (element "div" >=> attributeIs "class" "gs_a")) $ cur
    auths = map (Author . pruneSpaces) . wordsBy (==',') . head $ authJourYr
    jour = pruneSpaces . concat . init . wordsBy(==',') . head . tail $ authJourYr
    yr = read . pruneSpaces . last . wordsBy (==',') . head . tail $ authJourYr
    cites = case node. head . ($// element "a") . head .
        ($// (element "div" >=> attributeIs "class" "gs_fl")) $ cur of
            NodeElement e ->
                case M.toList . M.filterWithKey (\name _ -> nameLocalName name == "href") . elementAttributes $ e of
                    (_,url):_ -> "http://scholar.google.com" ++ T.unpack url
                    []        -> ""
            _             -> ""
    pruneSpaces = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')


-- converts tag tree to simple text
pruneTags :: Cursor -> String
pruneTags = T.unpack . pruneTags' . node
    where
    pruneTags' :: Node -> T.Text
    pruneTags' (NodeElement e) = T.concat . map pruneTags' $ elementNodes e
    pruneTags' (NodeContent c) = c
    pruneTags' _               = ""



{- TODO:
    form HTTP query for Google Scholar
    receive the resulting HTML page
    parse it to find the 1st paper and cites list
    return it
    crawling: internal limit on papers. Emm... somehow
-}
