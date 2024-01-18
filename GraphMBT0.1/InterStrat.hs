module InterStrat where 
import Utils
import Trails
import StrategyBas
import Rules
import Data.Maybe
import InterRules
import Refinement
import Data.Map (Map)
import qualified Data.Map as Map

test1 = start [14, 43, 64, 34, 97]
test2 = Map.singleton "inter" 100.34 :: Ref Rational
test3 = start [10, 43, 55, 34, 97]

lbl :: String -> Rule (Ref Rational)
lbl = label allRules

caldx :: Strat (Ref Rational)
caldx = R xdif <|>
        R x1dif <|>
        R x2dif 

caldy :: Strat (Ref Rational)
caldy = R ydif <|>
        R y1dif <|>
        R y2dif

caldyDdx :: Strat (Ref Rational)
caldyDdx = R dyDivdx .*.
           option (R negSlope) .*.
           option (R invSlope) .*.
           option (R slopeRound <|>
                   R slopeRoundB)

caldx1v :: Strat (Ref Rational)
caldx1v = R dx1xv <|>
          R dxv1

caldx2v :: Strat (Ref Rational)
caldx2v = R dx2xv <|>
          R dxv2

interpol1 :: Strat (Ref Rational)
interpol1 = R inter1 <|>
            R crossInter2 <|>
            R interNoBase1 <|>
            R interX1 <|>
            R crossInterX2 

interpol2 :: Strat (Ref Rational)
interpol2 = R inter2 <|>
            R crossInter1 <|>
            R interNoBase2 <|> 
            R interX2 <|>
            R crossInterX1

interFromCept :: Strat (Ref Rational)
interFromCept = (R callb1 <|>
                 R callb2) .*. 
                option (R roundCept <|>
                        R roundCeptB) .*.
                        R interFromB

finalRounding :: Strat (Ref Rational)
finalRounding = 
   option (R finalRound <|> 
           R finalRoundB)

negateFinalAns :: Strat (Ref Rational)
negateFinalAns = option (R negateFinal)

interpolation :: Strat (Ref Rational)
interpolation = 
   (caldx .*. 
   caldy .*. 
   caldyDdx .*. 
   ((caldx1v .*. interpol1) <|> 
   (caldx2v .*. interpol2) <|>
   (interFromCept)) <|> R calSeq) .*. 
   finalRounding .*. 
   negateFinalAns .*.
   toFFI

trunkStrat :: Strat (Ref Rational)
trunkStrat = 
   caldx .*. caldy .*. caldyDdx .*. R finalSlope

finalSlope :: Rule (Ref Rational)
finalSlope = rule "finSlope" f 
   where 
      f rf = 
         case "dy/dx" << rf 
           of Nothing   -> []
              Just ans  -> [Map.singleton "dy/dx" ans]

toFinalFormI :: Ref Rational -> [(Ref Rational)]
toFinalFormI  rf = 
    case "inter" << rf 
      of Nothing   -> []
         Just ans  -> [Map.singleton "inter" ans]

toFFI :: Strat (Ref Rational)
toFFI = R $ rule "toFinal" toFinalFormI    