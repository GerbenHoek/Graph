module BackTrack where
import BWSGraph 
import Utils hiding (nubSub)
import BWSGBuilder
import NubSub
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

type Tracker n e = (n, [L e])
type Trackers n e = Map n [L e]

embed :: (Ord n, Ord e) => [n] -> [Tracker n e]
embed ns = [(n, [emptyL]) | n <- ns]

insertT :: (Ord n, Ord e) => Tracker n e -> Trackers n e -> Trackers n e 
insertT (n, se) = Map.insertWith nubSub n se

buildT :: (Ord n, Ord e) => [Tracker n e] -> Trackers n e 
buildT = foldr insertT Map.empty

applyBWG :: (Ord s, Ord n, Ord e) => 
   (e -> Bool) -> BWSGraph s n e -> Tracker (s, n) e -> [Tracker (s, n) e]
applyBWG b g ((s, n), es) = case Map.lookup s g of
   Nothing  -> []
   Just nm -> 
      case Map.lookup n nm of
         Nothing  -> []
         Just se' -> 
            [((s', n'), ins e' es) | 
                ((s', n'), e') <- Set.toList se']
            where ins a sa = if b a 
                             then map (a|:) sa -- (|:) is (:) for lists in an L wrapper
                             else sa

backTrack :: (Ord s, Ord n, Ord e) => 
   (e -> Bool) -> BWSGraph s n e -> [(s, n)] -> Trackers (s, n) e
backTrack b g ns = fst $ iterWhileP1 (not.null.snd) 
   build' start' 
   where 
      start' = (Map.empty, Map.fromList $ embed ns) 
      build' (ot, nt) = (Map.unionWith nubSub ot nt, nt')
         where nt' = buildT $ concatMap (applyBWG b g) $ pts 
               pts = Map.toList $ 
                  Map.union (Map.intersectionWith nubSub ot nt) nt    