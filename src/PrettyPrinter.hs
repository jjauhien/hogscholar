-----------------------------------------------------------------------------
--
-- Module      :  PrettyPrinter
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

module PrettyPrinter  where

import Papers

-- FIXME: better output
prettify :: Papers -> String
prettify = show Papers
