module BWSGraph where
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

type PGraph s n e = Map n (Set ((s, n), e))
type BWSGraph s n e = Map s (PGraph s n e)

emptyBWS :: BWSGraph s n e
emptyBWS = Map.empty 

emptyEdges :: (Ord s, Ord n, Ord e) => 
   (s, n) -> BWSGraph s n e 
emptyEdges (s, n) = 
   Map.singleton s $ Map.singleton n (Set.empty)

singletonPG :: (Ord s, Ord n, Ord e) => 
   (s, n, n, e) -> PGraph s n e
singletonPG (s, n1, n2, e) = 
   Map.singleton n2 (Set.singleton ((s, n1), e))

singletonBWS :: (Ord s, Ord n, Ord e) => 
   ((s, n), (s, n), e) -> BWSGraph s n e
singletonBWS ((s1, n1), (s2, n2), e) = 
   Map.singleton s2 $ singletonPG (s1, n1, n2, e)

unionPG :: (Ord s, Ord n, Ord e) => 
   PGraph s n e -> PGraph s n e -> PGraph s n e
unionPG g1 g2 = Map.unionWith (Set.union) g1 g2

unionBWS :: (Ord s, Ord n, Ord e) => 
   BWSGraph s n e -> BWSGraph s n e -> BWSGraph s n e
unionBWS = Map.unionWith unionPG

insertBWS :: (Ord s, Ord n, Ord e) => 
   ((s, n), (s, n), e) -> BWSGraph s n e -> BWSGraph s n e
insertBWS ((s1, n1), (s2, n2), e) =
   Map.insertWith unionPG s2 (singletonPG (s1, n1, n2, e)) 

buildBWS :: (Ord s, Ord n, Ord e) => 
   [((s, n), (s, n), e)] -> BWSGraph s n e
buildBWS = foldr insertBWS emptyBWS

augmentBWS :: (Ord s, Ord n, Ord e) => 
   [((s, n), (s, n), e)] -> BWSGraph s n e -> BWSGraph s n e
augmentBWS egs g = foldr insertBWS g egs

pointSet :: (Ord s, Ord n, Ord e) => 
   BWSGraph s n e -> Set (s, n)
pointSet = Set.fromAscList . concatMap f . Map.toAscList
   where f (s, mn) = [(s, n) | n <- Map.keys mn]