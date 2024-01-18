module BWSGBuilder where
import Utils
import Data.Maybe
import FSM
import Rules
import BWSGraph
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

type P a = (Int, a)
type E a = (P a, P a, Maybe (Rule a))
type FSMR a = FSM Int (Rule a)
type RGraph a = BWSGraph Int a (Maybe (Rule a))  

apply :: Ord a => FSMR a -> P a -> [E a]
apply fsm (s, a) = 
   case Map.lookup s (transitions fsm) of 
      Nothing  -> error "state not in FSM"
      Just trs -> do 
         (mr, s') <- trs
         a'       <- fromMaybe [a] $ flip applyRule a <$> mr 
         return ((s, a), (s', a'), mr)

build :: Ord a => FSMR a -> a -> RGraph a
build fsm a = snd $ iterWhile (not.null.fst.fst) (build' fsm) start' 
   where 
      start' = (([(start fsm, a)], Set.empty), emptyBWS)
      build' fsm ((w@(s,a):wl, visited), g) =
         if    Set.member w visited 
         then  ((wl, visited), g)
         else  ((npts ++ wl, Set.insert w visited), augmentBWS new g)
         where new  = apply fsm w
               npts = [p2 | (p1, p2, mr) <- new]

build2 :: Ord a => FSMR a -> a -> RGraph a
build2 fsm a = fst $ iterWhile (not.null.snd) build' start' 
   where 
      start' = (emptyBWS, emptyEdges (start fsm, a))
      build' (og, ng) = (unionBWS og ng, ng')
         where ng' = buildBWS $ concatMap (apply fsm) $ pts
               pts = Set.toList $ 
                  Set.difference (pointSet ng) (pointSet og) 

finalAns :: Ord a => FSM Int (Rule a) -> a -> Set a
finalAns fsm a = Set.unions [getFinal s $ build fsm a | s <- accept fsm] 
   where
      getFinal s = Map.keysSet.fromJust.Map.lookup s