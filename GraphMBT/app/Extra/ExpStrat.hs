module Extra.ExpStrat where 
import Data.Maybe
import FSMMaker.Trails
import FSMMaker.StrategyBas
import FSMMaker.Rules
import Extra.ExpRules
import Extra.Refinement
import Data.Map (Map)
import qualified Data.Map as Map
import Utils

--[("x1",37 % 1),("x2",41 % 1),("xv",55 % 1),("y1",58 % 1),("y2",68 % 1)]

test11 = start [37, 41, 58, 68, 55]
test12 = Map.singleton "inter" 73.6 :: Ref Rational

calcFactorS :: Strat (Ref Rational)
calcFactorS = ((R calcFactor <|> R factorIsQuot) .*. 
              option (R invertFactor)) .*.
              option (R roundFactor <|> R roundFactorB)

calc_dxS :: Strat (Ref Rational)
calc_dxS = option $ R calc_dx <|> R dx_Is_x1 <|> R dx_Is_x2

calcGrowS :: Strat (Ref Rational)
calcGrowS = calc_dxS .*.
            (R calcGrow <|> R growNoRecip <|> R growDiv <|> R growNoExp) 
            
roundGrowS :: Strat (Ref Rational)
roundGrowS = option (R roundGrow3 <|> R roundGrow <|> R roundGrowB)

calcDy :: Strat (Ref Rational)
calcDy = 
   R factorIsdy .*. 
   calc_dxS .*.
   ((R calcGrow <|> R growNoRecip <|> R growDiv) .*.
   option (R invertGrow) <|> R growNoExp) 

calc_dx1xvS :: Strat (Ref Rational)
calc_dx1xvS = R calc_dx1xv <|> R dx1xv_Is_xv

calc_dx2xvS :: Strat (Ref Rational)
calc_dx2xvS = R calc_dx2xv <|> R dx2xv_Is_xv

inter1S :: Strat (Ref Rational)
inter1S = R inter1 

inter2S :: Strat (Ref Rational)
inter2S = R inter2 

interBS :: Strat (Ref Rational)
interBS = 
   ((R baseIs_y1 .*. (R newGrow1 <|> R newGrow1Mul <|> R newGrowIsg)) <|>
   (R baseIs_y2 .*. (R newGrow2 <|> R newGrow2Mul <|> R newGrowIsg)) <|>
   (R (toggleRule baseIs_y1) .*. (R newGrow2 <|> R newGrow2Mul)) <|>
   (R (toggleRule baseIs_y2) .*. (R newGrow2 <|> R newGrow2Mul))) .*. 
   (R interB <|> R interAdd)

interNoB :: Strat (Ref Rational)
interNoB = 
   R baseIs_1 .*. 
   (R newGrow1 <|> R newGrow2 <|> R newGrow1Mul <|> R newGrow2Mul <|> R newGrowIsg) .*.
   R interB 

finalS :: Strat (Ref Rational)
finalS = R finalAns .*.
         option (R finalRound3 <|> R finalRound <|> R finalRoundB)

exponS :: Strat (Ref Rational)
exponS = ((calcFactorS .*. calcGrowS) <|> calcDy) .*. 
         roundGrowS .*.
         (calc_dx1xvS <|> calc_dx2xvS) .*. 
         (interBS <|> interNoB) .*.
         finalS 

trunkStratE :: Strat (Ref Rational)
trunkStratE = 
   ((calcFactorS .*. calcGrowS) <|> calcDy) .*. 
   roundGrowG .*. 
   R finalGrow

roundGrowG :: Strat (Ref Rational)
roundGrowG = option (R (toggleRule roundGrow3) <|> R roundGrow <|> R roundGrowB)

finalGrow :: Rule (Ref Rational)
finalGrow = rule "finGrow" f 
   where 
      f rf = 
         case "g" << rf 
           of Nothing   -> []
              Just ans  -> [Map.singleton "g" ans]