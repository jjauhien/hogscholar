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


crawlPapers :: Query -> IO (Maybe Papers)
crawlPapers = undefined
