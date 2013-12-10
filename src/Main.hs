{-# LANGUAGE CPP, TemplateHaskell #-}
-----------------------------------------------------------------------------
--
-- Module      :  Main
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

module Main (
    main
) where

import Crawler
import Papers
import PrettyPrinter


main = undefined

-- TODO: processing of command line
-- maybe use: System.Console.GetOpt
-- (see http://www.haskell.org/haskellwiki/High-level_option_handling_with_GetOpt)
parseArgs :: IO Query
parseArgs = undefined
