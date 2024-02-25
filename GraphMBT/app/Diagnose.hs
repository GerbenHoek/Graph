module Diagnose where
import BackTracker.GraphBuilder (build, FSMR)
import BackTracker.BackTrack (backTrack, backTrack2)
import BackTracker.NubSub (L)
import FSMMaker.Rules
import FSMMaker.FSM (start, accept)
import Prune
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe

isBuggyE :: Maybe (Rule a) -> Bool
isBuggyE = (maybe False isBuggy)

--diagnose 
--   :: Ord a => 
--      FSMR a 
--      -> a -> a 
--      -> Maybe [[(Maybe (Rule a))]]
diagnose fsm a1 a2 =
   Map.lookup a1' $ 
   backTrack isBuggyE g pts 
   where 
      a1' = (start fsm, a1)
      pts = [(s, a2) | s <- accept fsm]
      g = build fsm a1   

diagnoseMax
  :: Ord a =>
     Int
     -> FSMR a 
     -> a -> a 
     -> Maybe [[(Maybe (Rule (Ct a)))]]
diagnoseMax n fsm a1 a2 =
   Map.lookup a1' $
   backTrack isBuggyE g pts 
   where 
      a1' = (start fsm, C (a1, 0))
      pts = [(s, C (a2, k)) | s <- accept fsm, k <- [0..n]]
      g = build (pruneBFSM n fsm) $ C (a1, 0) 