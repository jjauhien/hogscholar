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
import Data.Maybe

data Paper = Paper {
      authors :: [Author]
    , title   :: String
    , journal :: String
    , year    :: Int
    } deriving (Eq)

type Author = String

type PaperGraph = Gr Paper ()

{-data PaperGraphWrapper = PaperGraphWrapper {
        graph :: PaperGraph
    ,   pMap  :: M.Map Paper Node
    }  deriving (Eq)
 -}
 
type PaperGraphWrapper = (PaperGraph, M.Map Paper Node)

--showGr pg = showX (show showX  
 
--showGr pg = putStr $ unlines $ map (((++ "\n")) . show) (labNodes pg)
  --  where f (n, l) =  show l ++ showPL (pre $ context pg n)  

printGr pg = putStr $ unlMap showLNode (labNodes pg) 
        where
            showLNode(n, l)  = show l ++ "\nCited by:\n" ++ showCitForNode n
            showCitForNode n | not . null $ pre pg n   = unlMap (show . title . fromJust . lab pg)  (pre pg n) 
                             | otherwise               = "Noone"
                 
unlMap :: (a -> String) -> [a] -> String
unlMap f = unlines . map f

printList [x] s    = x
printList (x:xs) s = x ++ s ++ printList xs s

singleton :: Paper -> PaperGraphWrapper
singleton p = (mkGraph [(0, p)] [], M.singleton p 0)

instance Show Paper where
  show p    = "\nTitle: " ++ title p ++ "\nAuthors: " ++ showAuths ++ "\nJournal: " ++ journal p ++ "\nYear: " ++ show (year p) ++ "\n"
        where showAuths = printList(authors p) ", "
              
instance Ord Paper where
    p1 <= p2 = title p1 <= title p2

p1 = Paper ["Pelle Pärsson"] "Vektorer och matriser" "Matematisk tidskrift" 1953
p2 = Paper ["Hasse Hansson", "Gunnar Göransson"] "Blommor och bin" "Biologisk tidskrift" 1997
p3 = Paper ["Hasse Hansson", "Uffe Svensson"] "Bultar och skruvar" "Mekanisk tidskrift" 2001

pg' :: Gr Paper String

pg' = mkGraph [(1, p1), (2, p2), (3, p3)] [(2, 1, "21"), (3, 2, "32"), (3, 1, "31")]

addEdge :: PaperGraphWrapper -> Paper -> Paper ->  PaperGraphWrapper
addEdge (pg, m) p1 p2  = undefined
    where
    (pg', m') = addPaper p2 . addPaper p1 $ (pg, m)

    
addPaper :: Paper -> PaperGraphWrapper -> PaperGraphWrapper
addPaper p (pg, m) = case M.lookup p m of 
                        Just _ -> (pg, m)
                        Nothing -> (insNode (noNs, p) pg, M.insert p noNs m)
    where
    noNs = noNodes pg
    
insertIfNotPresent k a m = if k `M.notMember` m then M.insert k a m else m
 {-               
getCitations :: PaperGraph -> Node -> [Node]
getCitations = pre

getReferences :: PaperGraph -> Node -> [Node]
getReferences = suc

doesCite :: PaperGraph -> Node -> Node -> Bool
doesCite pg x y = x `elem` pre pg y

getPapersWhere :: (Paper -> Bool) -> PaperGraph -> [LNode Paper]
getPapersWhere f pg = filter (f . snd) (labNodes pg)

getPapersBy :: Author -> PaperGraph -> [LNode Paper]
getPapersBy a = getPapersWhere (\p -> a `elem` authors p)


haveCoauthored :: Author -> Author -> PaperGraph -> Bool
haveCoauthored a1 a2 pg = any hc (labNodes pg)
    where hc n = a1 `elem` as && a2 `elem` as
                where as = (authors . snd) n
    
            
mostCitedPaper :: PaperGraph -> Node
mostCitedPaper pg = fst $ foldr1 max' (map (\(n,_) -> (n, indeg pg n)) (labNodes pg))
    where  max' x y | snd x > snd y = x
                    | otherwise     = y

-}    
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
