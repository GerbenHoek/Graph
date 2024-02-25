module BackTracker.GraphBuilder where
import qualified BackTracker.Graph as G
import FSMMaker.FSM (FSM, transitions, start, accept)
import FSMMaker.Rules (Rule, applyRule)
import Utils (iterWhile)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe

type St = Int 
type N a = (St, a)
type E a = Maybe (Rule a)
type Seg a = (N a, E a, N a)
type FSMR a = FSM St (Rule a)
type RGraph a = G.Graph (N a) (E a)  

apply :: Ord a => FSMR a -> N a -> [Seg a]
apply fsm (s, a) = --if s `elem` accept fsm then [] else 
   case Map.lookup s (transitions fsm) of 
      Nothing  -> error "state not in FSM"
      Just trs -> do 
         (mr, s') <- trs
         a'       <- maybe [a] (flip applyRule a) mr 
         return ((s, a), mr, (s', a'))

build2 :: Ord a => FSMR a -> a -> RGraph a
build2 fsm a = snd $ iterWhile (not.null.fst.fst) (build' fsm) start' 
   where 
      start' = (([(start fsm, a)], Set.empty), G.empty)
      build' fsm ((w:wl, visited), g) =
         if    Set.member w visited 
         then  ((wl, visited), g)
         else  ((npts ++ wl, visited'), G.augment new g)
         where new  = apply fsm w
               visited' = Set.insert w visited
               npts = [p | (_, _, p) <- new]

build :: Ord a => FSMR a -> a -> RGraph a
build fsm a = g 
   where 
      (_, _, g) = build' ([(start fsm, a)], Set.empty, G.empty)
      build' ([], visited, g) = ([], visited, g)
      build' (w:wl, visited, g) = 
         if w `Set.member` visited
         then build' (wl, visited, g)
         else build' (npts ++ wl, visited', G.augment new g)
         where 
            new = apply fsm w 
            visited' = Set.insert w visited
            npts = [p | (_, _, p) <- new]