module InterRules where 
import Rules
import Refinement
import Data.Map (Map)
import qualified Data.Map as Map

rnd' :: Int -> Maybe Rational -> Maybe Rational
rnd' k a = (/10^k) . round' . (* 10^k) <$> a
   where 
      round' b = (signum b) * round'' (abs b)  
      round'' c  
         | c - tr floor c >= 1 / 2 = tr ceiling c
         | otherwise = tr floor c
      tr f = toRational . f

rnd :: Int -> Maybe Rational -> Maybe Rational
rnd k a 
   | r == a    = Nothing
   | otherwise = r  
   where 
      r = rnd' k a

rndB :: Int -> Maybe Rational -> Maybe Rational
rndB k a 
   | r == a       = Nothing
   | r == floor'' = Nothing
   | otherwise    = floor''
   where 
      r = rnd k a 
      floor'' = (/10^k) . floor' . (* 10^k) <$> a
      floor' b = signum b * (toRational . floor . abs) b

start :: [Rational] -> Ref Rational
start = Map.fromList . zip ["x1", "x2", "y1", "y2", "xv"] 

xdif :: Rule (Ref Rational)
xdif = rule "calculate: dx = x2 - x1" f 
   where 
      f rf 
         | isPresent "dx" rf = []
         | otherwise = ("dx", x2 - x1)|:rf
         where x1 = "x1" << rf
               x2 = "x2" << rf

x1dif :: Rule (Ref Rational)
x1dif = buggyRule "calculate: dx = x1" f 
   where 
      f rf 
         | isPresent "dx" rf = []
         | otherwise = ("dx", x1)|:rf
         where x1 = "x1" << rf

x2dif :: Rule (Ref Rational)
x2dif = buggyRule "calculate: dx = x2" f 
   where 
      f rf 
         | isPresent "dx" rf = []
         | otherwise = ("dx", x2)|:rf
         where x2 = "x2" << rf

ydif :: Rule (Ref Rational)
ydif = rule "calculate: dy = y2 - y1" f 
   where 
      f rf 
         | isPresent "dy" rf = []
         | otherwise = ("dy", y2 - y1)|:rf
         where y1 = "y1" << rf
               y2 = "y2" << rf

y1dif :: Rule (Ref Rational)
y1dif = buggyRule "calculate: dy = y1" f 
   where 
      f rf 
         | isPresent "dy" rf = []
         | otherwise = ("dy", y1)|:rf
         where y1 = "y1" << rf

y2dif :: Rule (Ref Rational)
y2dif = buggyRule "calculate: dy = y2" f 
   where 
      f rf 
         | isPresent "dy" rf = []
         | otherwise = ("dy", y2)|:rf
         where y2 = "y2" << rf

dyDivdx :: Rule (Ref Rational)
dyDivdx = rule "calculate: slope = dy/dx" f
   where 
      f rf 
         | isPresent "dy/dx" rf = []
         | otherwise = ("dy/dx", dy/dx)|:rf
         where dx = "dx" << rf
               dy = "dy" << rf

negSlope :: Rule (Ref Rational)
negSlope = buggyRule "negate slope" f
   where 
      f rf 
         | (not.isPresent "dy/dx") rf = []
         | otherwise = ("dy/dx", - dyDdx)!:rf
         where 
            dyDdx = "dy/dx" << rf

invSlope :: Rule (Ref Rational)
invSlope = buggyRule "invert slope" f
   where 
      f rf 
         | (not.isPresent "dy/dx") rf = []
         | otherwise = ("dy/dx", 1 / dyDdx)!:rf
         where 
            dyDdx = "dy/dx" << rf    

slopeRound :: Rule (Ref Rational)
slopeRound = buggyRule "round slope correctly" f
   where 
      f rf 
         | (not.isPresent "dy/dx") rf = [] 
         | otherwise = concat 
            [("dy/dx", rnd k dyDdx)!:rf | k <- [1..3]] 
         where 
            dyDdx = "dy/dx" << rf

slopeRoundB :: Rule (Ref Rational)
slopeRoundB = buggyRule "round slope incorrectly" f
   where 
      f rf 
         | (not.isPresent "dy/dx") rf = [] 
         | otherwise = concat 
            [("dy/dx", rndB k dyDdx)!:rf | k <- [1..3]] 
         where 
            dyDdx = "dy/dx" << rf

dx1xv :: Rule (Ref Rational)
dx1xv = rule "calculate: dx1xv = xv - x1" f
   where 
      f rf 
         | isPresent "dx1xv" rf = []
         | otherwise = ("dx1xv", xv - x1)|:rf
         where xv = "xv" << rf
               x1 = "x1" << rf

dxv1 :: Rule (Ref Rational)
dxv1 = buggyRule "calculate: dx1xv = xv" f
   where 
      f rf 
         | isPresent "dx1xv" rf = []
         | otherwise = ("dx1xv", xv)|:rf
         where xv = "xv" << rf

dx2xv :: Rule (Ref Rational)
dx2xv = rule "calculate: dx2xv = xv - x2" f
   where 
      f rf 
         | isPresent "dx2xv" rf = []
         | otherwise = ("dx2xv", xv - x2)|:rf
         where xv = "xv" << rf
               x2 = "x2" << rf

dxv2 :: Rule (Ref Rational)
dxv2 = buggyRule "calculate: dx2xv = xv" f
   where 
      f rf 
         | isPresent "dx2xv" rf = []
         | otherwise = ("dx2xv", xv)|:rf
         where xv = "xv" << rf

inter1 :: Rule (Ref Rational)
inter1 = rule "calculate: inter = y1 + dx1xv * dy/dx" f
   where 
      f rf 
         | isPresent "inter" rf = []
         | otherwise = ("inter", y1 + dx1xv * dyDdx)|:rf
         where dx1xv = "dx1xv" << rf
               dyDdx = "dy/dx" << rf
               y1    = "y1" << rf

interNoBase1 :: Rule (Ref Rational)
interNoBase1 = buggyRule "calculate: inter = dx1xv * dy/dx" f
   where 
      f rf 
         | isPresent "inter" rf = []
         | otherwise = ("inter", dx1xv * dyDdx)|:rf
         where dx1xv = "dx1xv" << rf
               dyDdx = "dy/dx" << rf

interX1 :: Rule (Ref Rational)
interX1 = buggyRule "calculate: inter = x1 + dx1xv * dy/dx" f
   where 
      f rf 
         | isPresent "inter" rf = []
         | otherwise = ("inter", x1 + dx1xv * dyDdx)|:rf
         where dx1xv = "dx1xv" << rf
               dyDdx = "dy/dx" << rf
               x1    = "x1" << rf

crossInter2 :: Rule (Ref Rational)
crossInter2 = buggyRule "calculate: inter = y2 + dx1xv * dy/dx" f
   where 
      f rf 
         | isPresent "inter" rf = []
         | otherwise = ("inter", y2 + dx1xv * dyDdx)|:rf
         where dx1xv = "dx1xv" << rf
               dyDdx = "dy/dx" << rf
               y2    = "y2" << rf

crossInterX2 :: Rule (Ref Rational)
crossInterX2 = buggyRule "calculate: inter = x2 + dx1xv * dy/dx" f
   where 
      f rf 
         | isPresent "inter" rf = []
         | otherwise = ("inter", x2 + dx1xv * dyDdx)|:rf
         where dx1xv = "dx1xv" << rf
               dyDdx = "dy/dx" << rf
               x2    = "x2" << rf

inter2 :: Rule (Ref Rational)
inter2 = rule "calculate: inter = y2 + dx2xv * dy/dx" f
   where 
      f rf 
         | isPresent "inter" rf = []
         | otherwise = ("inter", y2 + dx2xv * dyDdx)|:rf
         where dx2xv = "dx2xv" << rf
               dyDdx = "dy/dx" << rf
               y2    = "y2" << rf

interNoBase2 :: Rule (Ref Rational)
interNoBase2 = buggyRule "calculate: inter = dx2xv * dy/dx" f
   where 
      f rf 
         | isPresent "inter" rf = []
         | otherwise = ("inter", dx2xv * dyDdx)|:rf
         where dx2xv = "dx2xv" << rf
               dyDdx = "dy/dx" << rf

interX2 :: Rule (Ref Rational)
interX2 = buggyRule "calculate: inter = x2 + dx2xv * dy/dx" f
   where 
      f rf 
         | isPresent "inter" rf = []
         | otherwise = ("inter", x2 + dx2xv * dyDdx)|:rf
         where dx2xv = "dx2xv" << rf
               dyDdx = "dy/dx" << rf
               x2    = "x2" << rf

crossInter1 :: Rule (Ref Rational)
crossInter1 = buggyRule "calculate: inter = y1 + dx2xv * dy/dx" f
   where 
      f rf 
         | isPresent "inter" rf = []
         | otherwise = ("inter", y1 + dx2xv * dyDdx)|:rf
         where dx2xv = "dx2xv" << rf
               dyDdx = "dy/dx" << rf
               y1    = "y1" << rf

crossInterX1 :: Rule (Ref Rational)
crossInterX1 = buggyRule "calculate: inter = x1 + dx2xv * dy/dx" f
   where 
      f rf 
         | isPresent "inter" rf = []
         | otherwise = ("inter", x1 + dx2xv * dyDdx)|:rf
         where dx2xv = "dx2xv" << rf
               dyDdx = "dy/dx" << rf
               x1    = "x1" << rf

callb1 :: Rule (Ref Rational)
callb1 = rule "calculate: b = y1 - x1 * dy/dx" f
   where 
      f rf 
         | isPresent "b" rf = []
         | otherwise = ("b", y1 - x1 * dyDdx)|:rf
         where dyDdx = "dy/dx" << rf
               x1    = "x1" << rf
               y1    = "y1" << rf

callb2 :: Rule (Ref Rational)
callb2 = rule "calculate: b = y2 - x2 * dy/dx" f
   where 
      f rf 
         | isPresent "b" rf = []
         | otherwise = ("b", y2 - x2 * dyDdx)|:rf
         where dyDdx = "dy/dx" << rf
               x2    = "x2" << rf
               y2    = "y2" << rf

roundCept :: Rule (Ref Rational) 
roundCept = buggyRule "round y-intercept correctly" f
   where 
      f rf 
         | (not.isPresent "b") rf = [] 
         | otherwise = concat 
            [("b", rnd k b)!:rf | k <- [1..3]] 
         where 
            b = "b" << rf 

roundCeptB :: Rule (Ref Rational) 
roundCeptB = buggyRule "round y-intercept incorrectly" f
   where 
      f rf 
         | (not.isPresent "b") rf = [] 
         | otherwise = concat 
            [("b", rndB k b)!:rf | k <- [1..3]] 
         where 
            b = "b" << rf 

interFromB :: Rule (Ref Rational) 
interFromB = rule "calculate: inter = b + xv * dyDdx" f
   where 
      f rf 
         | isPresent "inter" rf = []
         | otherwise = ("inter", b + xv * dyDdx)|:rf
         where b     = "b" << rf
               xv    = "xv" << rf
               dyDdx = "dy/dx" << rf

calSeq :: Rule (Ref Rational) 
calSeq = buggyRule "calculate y as a sequence" f
   where 
      f rf 
         | isPresent "inter" rf = []
         | xv > x2            = ("inter", y2 + (y2 - y1))|:rf
         | xv < x2 && xv > x1 = ("inter", (y2 - y1)/2)|:rf
         | xv < x1            = ("inter", y1 - (y2 - y1))|:rf
         where xv = "xv" << rf
               x1 = "x1" << rf
               x2 = "x2" << rf  
               y1 = "y1" << rf
               y2 = "y2" << rf

finalRound :: Rule (Ref Rational)
finalRound = buggyRule "round final answer correctly" f
   where 
      f rf 
         | (not.isPresent "inter") rf = [] 
         | otherwise = concat 
            [("inter", rnd k inter)!:rf | k <- [1..3]] 
         where 
            inter = "inter" << rf

finalRoundB :: Rule (Ref Rational)
finalRoundB = buggyRule "round final answer incorrectly" f
   where 
      f rf 
         | (not.isPresent "inter") rf = [] 
         | otherwise = concat 
            [("inter", rndB k inter)!:rf | k <- [1..3]] 
         where 
            inter = "inter" << rf

negateFinal :: Rule (Ref Rational)
negateFinal = buggyRule "negate final answer" f
   where 
      f rf 
         | (not.isPresent "inter") rf = [] 
         | otherwise = ("inter", -1 * inter)!:rf 
         where 
            inter = "inter" << rf

finalAns :: Rule (Ref Rational)
finalAns = rule "get final asnwer" f 
   where 
      f rf = case "inter" <<  rf 
               of Nothing  -> []
                  Just ans -> [Map.singleton "inter" ans]

rules :: [Rule (Ref Rational)]
rules = 
   [xdif,
    ydif,
    dyDivdx,
    dx1xv,
    dx2xv,
    inter1,
    inter2,
    finalAns,
    callb1,
    callb2,
    interFromB]

buggyRules :: [Rule (Ref Rational)]
buggyRules = 
   [x1dif,
    x2dif,
    y1dif,
    y2dif,
    invSlope,
    negSlope,
    dxv1,
    dxv2,
    interNoBase1,
    interNoBase2,
    interX1,
    interX2,
    crossInter1,
    crossInter2,
    crossInterX1,
    crossInterX2,
    roundCept,
    roundCeptB,
    calSeq,
    negateFinal,
    finalRound,
    finalRoundB,
    slopeRound,
    slopeRoundB
    ]

allRules :: [Rule (Ref Rational)]
allRules = rules ++ buggyRules
