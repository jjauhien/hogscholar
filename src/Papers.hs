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

module Papers where

--import Data.Array
--import Data.Graph
import Data.Graph.Inductive
import Data.Graph.Inductive.Tree
import qualified Data.Graph.Inductive.Graph as IG
import qualified Data.Map as M
import Data.List

data Paper = Paper {
      authors :: [Author]
    , title   :: String
    , journal :: String
    , year    :: Int
    } deriving (Eq, Show)

newtype Author = Author String
    deriving (Eq,Show)


{-  
data PaperGraph = PaperGraph {
        graph = 
-}
    
type PaperGraph = Gr Paper String

--instance Show Paper where
  --  show p = title p ++ ", " ++ 


p1 = Paper (auths["Pelle"]) "Vektorer och matriser" "Matematisk tidskrift" 1953
p2 = Paper (auths ["Hasse", "Gunnar"]) "Blommor och bin" "Biologisk tidskrift" 1997
p3 = Paper (auths ["Hasse", "Uffe"]) "Bultar och skruvar" "Mekanisk tidskrift" 2001

auths :: [String] -> [Author]
auths = map Author

pg' :: Gr Paper String

pg' = mkGraph [(1, p1), (2, p2), (3, p3)] [(2, 1, "21"), (3, 2, "32"), (3, 1, "31")]

addEdge :: Paper -> Paper -> PaperGraph -> PaperGraph
addEdge = undefined
                
getCitations :: PaperGraph -> Node -> [Node]
getCitations = pre

getReferences :: PaperGraph -> Node -> [Node]
getReferences = suc

doesCite :: PaperGraph -> Node -> Node -> Bool
doesCite pg x y = x `elem` pre pg y

getPapersWhere :: (Paper -> Bool) -> PaperGraph -> [LNode Paper]
getPapersWhere f pg = filter (f . snd) (labNodes pg)

getPapersBy :: Author -> PaperGraph -> [LNode Paper]
getPapersBy a = getPapersWhere (\p -> a `elem` (authors p))

haveCoauthored :: Author -> Author -> PaperGraph -> Bool
haveCoauthored a1 a2 pg = any hc (labNodes pg)
    where hc n = a1 `elem` as && a2 `elem` as
                where as = (authors . snd) n
    
            
mostCitedPaper :: PaperGraph -> Node
mostCitedPaper pg = fst $ foldr1 max' (map (\(n,_) -> (n, indeg pg n)) (labNodes pg))
    where  max' x y | snd x > snd y = x
                    | otherwise     = y

    
--instance Show PaperGraph where
  --  show p = show (graph  p)


addToGraph :: PaperGraph -> (Paper, [Paper]) -> PaperGraph
addToGraph = undefined

{- TODO:
    Given results of crawler, build a graph.
    QuickCheck tests?
    prune the tree according to "importance" of the paper
        (like citations size, etc.)

-}
