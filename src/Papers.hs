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

import Data.Graph.Inductive
import Data.Graph.Inductive.Tree
import qualified Data.Graph.Inductive.Graph as IG
import qualified Data.Map as M
import Data.List
import Data.Maybe
import Test.QuickCheck

data Paper = Paper {
      authors :: [Author]
    , title   :: String
    , journal :: String
    , year    :: Int
    } deriving (Eq)

type Author = String

type PaperGraph = Gr Paper ()

type PaperGraphWrapper = (PaperGraph, M.Map Paper Node)

newtype FirstName = FirstName String
newtype LastName = LastName String

--instance Arbitrary Paper where
--    arbitrary = Paper (oneof [["a"]]) (oneof ["a"])  (oneof ["a"])  (choose (1890, 2013))

instance Show Paper where
  show p    = show (title p) ++ ". " ++ showAuths ++ ". " ++ journal p ++ ", " ++ show (year p) ++ "\n"
        where showAuths = showList'(authors p) ", "
              
instance Ord Paper where
    p1 <= p2 = title p1 <= title p2

--Print a PaperGraph
printGr :: PaperGraph -> IO ()
printGr pg = putStr $ unlMap showLNode (labNodes pg) 
        where
            showLNode(n, l)  = "\n" ++ show l ++ "Cites: " ++ showRefForNode n ++ "\nCited by: " ++ showCitForNode n
            showCitForNode   = showNeiForNode pre 
            showRefForNode   = showNeiForNode suc 
            showNeiForNode f n | (not . null) nei  = showList' (map (show . title . fromJust . lab pg) nei) "," 
                               | otherwise  = "Noone"
                                where nei = reverse $ f pg n -- list of either predecessors or successors
                 
unlMap :: (a -> String) -> [a] -> String
unlMap f = unlines . map f


showList' [x] s    = x
showList' (x:xs) s = x ++ s ++ showList' xs s

-- Return a PaperGraphWrapper with a single node
singleton :: Paper -> PaperGraphWrapper
singleton p = (mkGraph [(0, p)] [], M.singleton p 0)

-- Add an edge to the graph
addEdge :: PaperGraphWrapper -> Paper -> Paper ->  PaperGraphWrapper
addEdge (pg, m) p2 p1  = (insEdge (n1, n2, ()) pg', m')
    where
    (pg', m') = addPaper p2 . addPaper p1 $ (pg, m)
    n1 = fromJust $ M.lookup p1 m'
    n2 = fromJust $ M.lookup p2 m'

-- Add a paper node to the graph
addPaper :: Paper -> PaperGraphWrapper -> PaperGraphWrapper
addPaper p (pg, m) = case M.lookup p m of 
                        Just _ -> (pg, m)
                        Nothing -> (insNode (noNs, p) pg, M.insert p noNs m)
    where
    noNs = noNodes pg

-- Get papers that cites this paper
getCitations :: PaperGraph -> Node -> [Node]
getCitations = pre


-- Get papers that this paper has cited
getReferences :: PaperGraph -> Node -> [Node]
getReferences = suc

-- Does paper x cite paper y
doesCite :: PaperGraph -> Node -> Node -> Bool
doesCite pg x y = x `elem` pre pg y

-- Get papers that fulfulls condition f
getPapersWhere :: (Paper -> Bool) -> PaperGraph -> [LNode Paper]
getPapersWhere f pg = filter (f . snd) (labNodes pg)

-- Get all papers by author a
getPapersBy :: Author -> PaperGraph -> [LNode Paper]
getPapersBy a = getPapersWhere (\p -> a `elem` authors p)

-- Have authors a1 and a2 written any papers together
haveCoauthored :: Author -> Author -> PaperGraph -> Bool
haveCoauthored a1 a2 pg = any hc (labNodes pg)
    where hc n = a1 `elem` as && a2 `elem` as
                where as = (authors . snd) n
    
-- Get paper with the highest number of citations   
mostCitedPaper :: PaperGraph -> Node
mostCitedPaper pg = fst $ foldr1 max' (map (\(n,_) -> (n, indeg pg n)) (labNodes pg))
    where  max' x y | snd x > snd y = x
                    | otherwise     = y

-- Used for testing

-- If there exists a paper with more than one author, there should exist at least one pair of coauthors 
prop_auths :: PaperGraph -> Bool
prop_auths pg = (not . null) (getPapersWhere (\p -> (length . authors) p > 1) pg) == or[haveCoauthored x y pg | x <- auths, y <- auths]
    where auths = nub $ concatMap (authors . snd) (labNodes pg)
                    
p1 = Paper ["Pelle Pärsson"] "Vektorer och matriser" "Matematisk tidskrift" 1953
p2 = Paper ["Hasse Hansson", "Gunnar Göransson"] "Blommor och bin" "Biologisk tidskrift" 1997
p3 = Paper ["Hasse Hansson", "Uffe Svensson"] "Bultar och skruvar" "Mekanisk tidskrift" 2001

pg' :: PaperGraph

pg' = mkGraph [(1, p1), (2, p2), (3, p3)] [(2, 1, ()), (3, 2, ()), (3, 1, ())]