module BackTracker.BackTrack where
import Data.Maybe (fromMaybe)
import Data.List (sort)
--import BackTracker.NubSub
import BackTracker.Graph (Graph)
import qualified BackTracker.Graph as G
import Utils 
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

type Tracker n e = (n, [[e]])
type Trackers n e = Map n [[e]]

embed :: (Ord n, Ord e) => [n] -> [Tracker n e]
embed ns = [(n, [[]]) | n <- ns]

insertT :: (Ord n, Ord e) => Tracker n e -> Trackers n e -> Trackers n e 
insertT (n, se) = Map.insertWith ins n se
   where ins as bs = nubSub (as ++ bs)

augmentT :: (Ord n, Ord e) => [Tracker n e] -> Trackers n e -> Trackers n e 
augmentT tkL tks = foldr insertT tks tkL 

buildT :: (Ord n, Ord e) => [Tracker n e] -> Trackers n e 
buildT = foldr insertT Map.empty

isCycle :: (Ord n, Ord e) => Tracker n e -> Trackers n e -> Bool
isCycle (n, es) trs = 
   maybe False (isCycle' es) (Map.lookup n trs) 
   where 
      isCycle' new old = 
         nubSub (new ++ old) == old 

applyG:: (Ord n, Ord e) => 
   (e -> Bool) -> Graph n e -> Tracker n e -> [Tracker n e]
applyG b g (n, es) = case G.backwards n g of
   Nothing  -> []
   Just se' -> 
      [(n', ins e' es) | (e', n') <- Set.toList se']
            where ins a sa = if b a 
                             then map (a:) sa 
                             else sa

--backTrack2 :: (Ord n, Ord e) => 
--   (e -> Bool) -> Graph n e -> [n] -> Trackers n e
backTrack2 n b g ns = snd $ last $ take n $ iterate backTrack' (embed ns, Map.empty)
   where 
      backTrack' ([], tks)   = ([], tks)
      backTrack' (w:wl, tks) = (new ++ wl, insertT w tks)
         where new = applyG b g w 

backTrack :: (Ord n, Ord e) => 
   (e -> Bool) -> Graph n e -> [n] -> Trackers n e
backTrack b g ns = snd $ backTrack' (embed ns, Map.empty)
   where 
      backTrack' ([], tks)   = ([], tks)
      backTrack' (w:wl, tks) = 
         if isCycle w tks then backTrack' (wl, tks)
         else backTrack' (new ++ wl, insertT w tks)
         where new = applyG b g w 