module Main (
    main
) where

import System.Environment (getArgs)
import System.Console.GetOpt

import Crawler
import Papers
import PrettyPrinter

main = do
    (l,q) <- parseInput
    print (l,q)
    mpg <- crawlPapers q l
    case mpg of
        Nothing -> ioError . userError $ "graph was not build :("
        Just pgw -> printGr (fst pgw)

-----------------------------------------------------------------------------
-- command-line arguments processing
-----------------------------------------------------------------------------

data Hogarg = Title String
          | Journal String
          | Year Int
          | Authors String
          | Levels Int
          deriving Show

options :: [OptDescr Hogarg]
options = [ Option [] ["title"]   (ReqArg Title "TITLE")       "paper's title"
          , Option [] ["journal"] (ReqArg Journal "JOURNAL")   "published in"
          , Option [] ["year"]    (ReqArg (Year . read) "YEAR")
                   "year of publication"
          , Option [] ["authors"] (ReqArg Authors "AUTHORS")
                   "authors (several such options could be given"
          , Option [] ["levels"]  (ReqArg (Levels . read) "LEVELS")
                   "nesting of the search"
          ]


parseInput :: IO (Int,Query)
parseInput = do
    args <- getArgs
    case getOpt RequireOrder options args of
        (os, _, []) -> return $ foldl updateArg (1,emptyQuery) os
        (_,  _, es) -> ioError (userError (concat es ++ usageInfo header options))
    where
        header = "Usage: hogscholar [OPTION...]"
        updateArg (l, Query as t j y) arg = case arg of
            Authors a  -> (l,  Query (a:as) t         j         y)
            Title   t' -> (l,  Query as     (Just t') j         y)
            Journal j' -> (l,  Query as     t         (Just j') y)
            Year    y' -> (l,  Query as     t         j         (Just y'))
            Levels  l' -> (l', Query as     t         j         y)

