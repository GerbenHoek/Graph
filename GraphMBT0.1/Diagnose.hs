module Diagnose where
import Expressions
import Data.Maybe
import BWSGBuilder
import BackTrack
import Prune
import Rules
import NubSub
import FSM 
import SimplinStrat
import InterStrat
import StrategyBas
import qualified Data.Map as Map
import qualified Data.Set as Set

isBuggyE :: Maybe (Rule a) -> Bool
isBuggyE = fromMaybe False . fmap isBuggy

diagnose 
   :: Ord a => 
      FSMR a 
      -> a -> a 
      -> Maybe [L (Maybe (Rule a))]
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
     -> Maybe [L (Maybe (Rule (Ct a)))]
diagnoseMax n fsm a1 a2 =
   Map.lookup a1' $ 
   backTrack isBuggyE g pts 
   where 
      a1' = (start fsm, C (a1, 0))
      pts = [(s, C (a2, 0)) | s <- accept fsm]
      g = build (pruneBFSM n fsm) $ C (a1, 0) 

-- run calculates the buggyRules in the shorted path between 
-- expr1 = 4 - 9 * (9 + 7 * Var "x") - Var "x" and 
-- expr2 = (-45) + 6 * Var "x"
-- runMax calcultes these rules only looking at paths that 
-- have no more than 5 buggyRules

run :: Maybe [L (Maybe (Rule Expr))]
run = diagnose (fsm simplify) expr1 expr2 

runMax :: Int -> Maybe [L (Maybe (Rule (Ct Expr)))]
runMax n = diagnoseMax n (fsm simplify) expr1 expr2 