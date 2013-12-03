-----------------------------------------------------------------------------
--
-- Module      :  Papers
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

module Papers (
    Paper, Author, Papers
) where

--import Data.Array
import Data.Graph


data Paper = Paper {
      authors :: [Author]
    , title   :: String
    , journal :: String
    , year    :: Int
    } deriving (Eq,Show)

newtype Author = Author String
    deriving (Eq,Show)

type References = [Paper]

data Papers = Papers -- (Graph, Array Int Paper)
    deriving (Show)
