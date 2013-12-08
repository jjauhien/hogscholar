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
import Data.Graph
import qualified Data.Map as M

data Paper = Paper {
      authors :: [Author]
    , title   :: String
    , journal :: String
    , year    :: Int
    } deriving (Eq,Show)

newtype Author = Author String
    deriving (Eq,Show)

--type Key = Int
--type Node = String
type References = [Paper]



{-data PaperGraph = PaperGraph {
    graph       :: Graph,
    aboutVertex :: Vertex -> ([Char], Int, [Int]),
    getVertex   :: Int -> Maybe Vertex
    }
-}    

    
instance Show PaperGraph where
    show p = show (graph  p)


addToGraph :: PaperGraph -> (Paper, [Paper]) -> PaperGraph
addToGraph = undefined

{- TODO:
    Given results of crawler, build a graph.
    QuickCheck tests?
    prune the tree according to "importance" of the paper
        (like citations size, etc.)

-}

--buildPaperGraph :: PaperGraph
{-buildPaperGraph = PaperGraph (fst3 gfe) (snd3 gfe) (trd3 gfe)
    where gfe =  graphFromEdges [("a", 0, []), ("b", 1, [0]), ("c", 2, [0,1])]
-}

fst3 (a,b,c) = a
snd3 (a,b,c) = b
trd3 (a,b,c) = c



data PaperGraph = PaperGraph {
    graph       :: Graph,
    pMap         :: M.Map Int Paper
    }

emptyGraph :: PaperGraph
emptyGraph = buildPaperGraph [] M.empty 

buildPaperGraph l = PaperGraph (g l)

--allVerticesMapped :: [Integer] -> Map Integer Paper -> Bool
--allVerticesMapped vs m = all (\k -> member k m) vs

allVerticesMapped :: PaperGraph -> Bool
allVerticesMapped (PaperGraph g m) = all (`M.member` m) (vertices g)

maxl :: (Ord a, Num a) => [(a,a)] -> a 
maxl l | null l    = 0
       | otherwise = maximum $ map maxt l
            where maxt (a,b) = max a b
    
v = [(i,j) | i <- [0..3], j <- [0..3], i > j]
gr = buildPaperGraph v (M.fromList [(i, Paper [Author "Pelle"] "Linjär algebra" "The Journal" 2013 ) | i <- [0..3]])

fi :: (Integer, Integer) -> (Int, Int)    
fi (a,b) = (fromIntegral a, fromIntegral b)

lookupPaper :: PaperGraph -> Vertex -> Paper
lookupPaper (PaperGraph _ m) v = m M.! v 

--getPapersBy :: PaperGraph -> Author -> [Vertex]
--getPapersBy (PaperGraph g m) a =  [(v, lookupPaper v)  | v <- vertices g, lookupPaper]       

--filter (\p -> a `elem` authors p) zip vs (map lookupPaper vs)
  --  where vs = vertices g

-- Does paper x cite paper x?
doesCite :: PaperGraph -> Vertex -> Vertex -> Bool
doesCite (PaperGraph g _ ) x y = (x,y) `elem` edges g

-- Get all the papers that cites this paper
getCitations :: PaperGraph -> Int -> [Vertex]
getCitations = getl fst snd

-- Get all the papers that this paper cites
getReferences :: PaperGraph -> Int -> [Vertex]
getReferences = getl snd fst

type TupleElem = ((Vertex,Vertex) -> Vertex)

getl :: TupleElem -> TupleElem -> PaperGraph -> Vertex -> [Vertex]
getl f f' (PaperGraph g m) v = map f (filter (\e -> f' e == v) (edges g))

--g :: [Edge] -> Graph
g l = buildG (0, maxl l') l'
    where l' = map fi l
