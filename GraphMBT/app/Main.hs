module Main where
import Diagnose (diagnose)
import qualified ApplyAll as A
import BackTracker.GraphBuilder --temp
import FSMMaker.StrategyBas -- temp
import SimpExpr.SimpLinStrat 
import Extra.ExpStrat
import Logic.LogicStrat
import Derive.Deriver
import FSMMaker.FSM hiding (build) --(fsm, merge)

main :: IO ()
main = do 
   --print $ show $ A.diagnose (exponS) test11 test12 
   --print $ show $ diagnose (merge $ fsm exponS) test11 test12 
   --print $ show $ diagnose (merge $ fsm toDNF) testi3 testi5
   --print $ show $ A.diagnose (toDNF) testi1 testi2
   --print $ show $ diagnose (merge $ fsm simplify) expr1 expr2
   --print $ show $ A.diagnose simplify expr1 expr2
   print $ show $ diagnose (merge $ fsm divStrat) deriv1 deriv2
   --print $ show $ A.diagnose (divStrat) deriv1 deriv2 
