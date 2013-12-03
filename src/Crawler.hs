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

module Crawler (
    Query
) where

import Papers

data Query = Query

-- downloads paper and the papers it refer
downloadPaper :: Query -> IO (Maybe (Paper,[Paper]))
downloadPaper = undefined


crawlPapers :: Query -> IO (Maybe PaperGraph)
crawlPapers = undefined

{- TODO:
    form HTTP query for Google Scholar
    receive the resulting HTML page
    parse it to find the 1st paper and cites list
    return it
    crawling: internal limit on papers. Emm... somehow
-}
