{-# LANGUAGE OverloadedStrings #-}

module Crawler (
      crawlPapers
    , Query(Query)
    , emptyQuery
) where

import qualified Data.ByteString.Lazy.Char8 as BS
import Data.List
import Data.List.Split (wordsBy, splitOn)
import Data.Maybe
import Data.Functor ((<$>))
import qualified Data.Map as Map
import qualified Data.Text as Text

import Control.Monad (foldM)
import Control.Monad.Trans.Class (lift)

import Network.CGI.Protocol (formEncode)
import Network.HTTP
import Network.URI (parseURI)

import Text.HTML.DOM (parseLBS)
import Text.XML hiding (parseLBS)
import Text.XML.Cursor

import Papers

-----------------------------------------------------------------------------

data Query = Query {
      qAuthors :: [Author]
    , qTitle   :: Maybe String
    , qJournal :: Maybe String
    , qYear    :: Maybe Int
    }

emptyQuery = Query [] Nothing Nothing Nothing

paper2query :: Paper -> Query
paper2query p = Query (authors p)
                      (Just $ title p)
                      (Just $ journal p)
                      (Just $ year p)

-----------------------------------------------------------------------------
-- Networking
-----------------------------------------------------------------------------

-- recursivyle requests information about papers,
--  not more than lvl levels
crawlPapers :: Query -> Int -> IO (Maybe PaperGraph)
crawlPapers q lvl = do
    r <- downloadPaper . formRequest $ q
    case r of
        Nothing     -> return Nothing
        Just (p,ps) -> Just <$> crawl (singleton p) (p,ps) lvl
    where
    singleton p = undefined
    crawl :: PaperGraph -> (Paper,[Paper]) -> Int -> IO PaperGraph
    crawl _  _ lvl | lvl < 0 = error "crawlPapers: level is negative"
    crawl pg _      0   = return pg
    crawl pg (p,ps) lvl = do
        subPs <- catMaybes <$> mapM (downloadPaper . formRequest . paper2query) ps
        let pg' = foldl (\accPg pap -> addEdge p pap accPg) pg ps
        foldM (\accPg (p,ps) -> crawl accPg (p,ps) (lvl-1)) pg' subPs


-- tries to download the paper and the papers which directly refer it
downloadPaper :: Request String -> IO (Maybe (Paper,[Paper]))
downloadPaper q = do
    s <- downloadHTML q
    case s >>= listToMaybe . parsePage of
        Nothing -> return Nothing
        Just (pap, req) -> do
            resp <- downloadHTML req
            case resp of
                Nothing -> return $ Just (pap, [])
                Just s1 -> return $ Just (pap, map fst . parsePage $ s1)


-- tries to download HTML page
downloadHTML :: Request String -> IO (Maybe String)
downloadHTML link = do
    resp <- simpleHTTP link
    case resp of
        Left _  -> return Nothing   -- some error
        Right r -> case rspCode r of
                    (2,_,_) -> return $ Just (rspBody r)
                    _       -> return Nothing  -- e.g. redirect


-- forms HTTP request from Query
formRequest :: Query -> Request String
formRequest (Query authors title journal year) =
        getRequest url
    where
    url = "http://scholar.google.com/scholar?" ++ params
    params = formEncode $ catMaybes
                    [ formKV "as_q" title
                    , onlyK "as_epq"
                    , onlyK "as_oq"
                    , onlyK "as_eq"
                    , formKV "as_occt" (Just "title")
                    , formKV "as_sauthors" (Just . intercalate ", " $ authors)
                    , formKV "as_publication" journal
                    , formKV "as_ylo" (show `fmap` year)
                    , formKV "as_yhi" (show `fmap` year)
                    , onlyK "btnG"
                    , formKV "hl" (Just "en")
                    , formKV "as_sdt" (Just "0,5")
                    ]
    formKV key val = (\v -> (key,v)) <$> val
    onlyK  key     = formKV key (Just "")


dummyQuery = Query []
                    (Just "A 480mb/s LDPC-COFDM-based UWB baseband transceiver")
                    Nothing Nothing
dummyRequest = formRequest dummyQuery
dummyPage = downloadHTML dummyRequest

-----------------------------------------------------------------------------
-- HTML/XML parsing
-----------------------------------------------------------------------------

-- parses all the papers on the results page
-- returns a list (Paper, Request to get list of citators)
parsePage :: String -> [(Paper, Request String)]
parsePage = map node2paper . ($// findPaperNodes) . fromDocument .
            parseLBS . BS.pack


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
    auths = map pruneSpaces . wordsBy (==',') . head $ authJourYr
    jour = pruneSpaces . concat . init . wordsBy(==',') . head . tail $ authJourYr
    yr = read . pruneSpaces . last . wordsBy (==',') . head . tail $ authJourYr
    cites = case node. head . ($// element "a") . head .
        ($// (element "div" >=> attributeIs "class" "gs_fl")) $ cur of
            NodeElement e ->
                case Map.toList .
                     Map.filterWithKey (\name _ -> nameLocalName name == "href") .
                     elementAttributes $ e of
                    (_,url):_ -> "http://scholar.google.com" ++ Text.unpack url
                    []        -> ""
            _             -> ""
    pruneSpaces = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')


-- converts tag tree to a plain text without any tags
pruneTags :: Cursor -> String
pruneTags = Text.unpack . pruneTags' . node
    where
    pruneTags' :: Node -> Text.Text
    pruneTags' (NodeElement e) = Text.concat . map pruneTags' . elementNodes $ e
    pruneTags' (NodeContent c) = c
    pruneTags' _               = ""

