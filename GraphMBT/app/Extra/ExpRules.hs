module Extra.ExpRules where
import Extra.Refinement
import FSMMaker.Rules 
import Utils
import Control.Monad (join)
import qualified Data.Map as Map

(^.) :: Maybe Rational -> Maybe Rational -> Maybe Rational
a ^. b = join $ power <$> a <*> b

rnd :: Int -> Maybe Rational -> Maybe Rational
rnd n a = rounder n <$> a 

rndB :: Int -> Maybe Rational -> Maybe Rational
rndB n a = roundDown n <$> a
   
start :: [Rational] -> Ref Rational
start = Map.fromList . zip ["x1", "x2", "y1", "y2", "xv"] 

calcFactor :: Rule (Ref Rational)
calcFactor = rule "calculate: f = y2/y1" f 
   where 
      f rf 
         | isPresent "f" rf = []
         | otherwise = ("f", y2/y1)|:rf
         where y1 = "y1" << rf
               y2 = "y2" << rf

factorIsdy :: Rule (Ref Rational)
factorIsdy = buggyRule "calculate: f = y2-y1 or f = y1-y2" f
   where 
      f rf 
         | isPresent "f" rf = []
         | otherwise = ("f", y2-y1)|:rf ++ ("f", y1-y2)|:rf
         where y1 = "y1" << rf
               y2 = "y2" << rf

factorIsQuot :: Rule (Ref Rational)
factorIsQuot = buggyRule "calculate: f = y1/x1 or f = y2/x2" f 
   where 
      f rf 
         | isPresent "f" rf = []
         | otherwise = ("f", y1/x1)|:rf ++ ("f", y2/x2)|:rf 
         where x1 = "x1" << rf
               x2 = "x2" << rf
               y1 = "y1" << rf
               y2 = "y2" << rf

invertFactor :: Rule (Ref Rational)
invertFactor = buggyRule "invert factor" f 
   where 
      f rf 
         | (not.isPresent "f") rf = []
         | otherwise = ("f", 1/f')|:rf 
         where f' = "f" << rf

roundFactor :: Rule (Ref Rational)
roundFactor = buggyRule "round factor correctly" f
   where 
      f rf 
         | (not.isPresent "f") rf = [] 
         | otherwise = concat 
            [("f", rnd k f')!:rf | k <- [1..3]] 
         where 
            f' = "f" << rf

roundFactorB :: Rule (Ref Rational)
roundFactorB = buggyRule "round factor incorrectly" f
   where 
      f rf 
         | (not.isPresent "f") rf = [] 
         | otherwise = concat 
            [("f", rndB k f')!:rf | k <- [1..3]] 
         where 
            f' = "f" << rf

calc_dx :: Rule (Ref Rational)
calc_dx = rule "calculate: dx = x2 - x1" f 
   where 
      f rf 
         | isPresent "dx" rf = []
         | otherwise = ("dx", x2 - x1)|:rf 
         where x1 = "x1" << rf
               x2 = "x2" << rf
             
dx_Is_x1 :: Rule (Ref Rational)
dx_Is_x1= buggyRule "calculate: dx = x1" f 
   where 
      f rf 
         | isPresent "dx" rf = []
         | otherwise = ("dx", x1)|:rf 
         where x1 = "x1" << rf

dx_Is_x2 :: Rule (Ref Rational)
dx_Is_x2 = buggyRule "calculate: dx = x2" f 
   where 
      f rf 
         | isPresent "dx" rf = []
         | otherwise = ("dx", x2)|:rf 
         where x2 = "x2" << rf

calcGrow :: Rule (Ref Rational)
calcGrow = rule "calculate: g = f^(1/dx)" f 
   where 
      f rf 
         | isPresent "g" rf = []
         | otherwise = ("g", f' ^. (1/dx))|:rf 
         where dx = "dx" << rf
               f' = "f"  << rf

growNoRecip :: Rule (Ref Rational)
growNoRecip = buggyRule "calculate: g = f^dx" f 
   where 
      f rf 
         | isPresent "g" rf = []
         | otherwise = ("g", f'^. dx)|:rf 
         where dx = "dx" << rf
               f' = "f"  << rf

growDiv :: Rule (Ref Rational)
growDiv = buggyRule "calculate: g = f/dx" f 
   where 
      f rf 
         | isPresent "g" rf = []
         | otherwise = ("g", f'/dx)|:rf 
         where dx = "dx" << rf
               f' = "f"  << rf

growNoExp :: Rule (Ref Rational)
growNoExp = buggyRule "calculate: g = f" f 
   where 
      f rf 
         | isPresent "g" rf = []
         | otherwise = ("g", f')|:rf 
         where f' = "f"  << rf

invertGrow :: Rule (Ref Rational)
invertGrow = buggyRule "invert grow" f 
   where 
      f rf 
         | (not.isPresent "g") rf = []
         | otherwise = ("g", 1/g)|:rf 
         where g = "g" << rf

roundGrow :: Rule (Ref Rational)
roundGrow = buggyRule "round g correctly" f
   where 
      f rf 
         | (not.isPresent "g") rf = [] 
         | otherwise = concat 
            [("g", rnd k g)!:rf | k <- [1,2]] 
         where 
            g = "g" << rf 

roundGrow3 :: Rule (Ref Rational)
roundGrow3 = buggyRule "round g correctly 3" f
   where 
      f rf 
         | (not.isPresent "g") rf = [] 
         | otherwise = ("g", rnd 3 g)!:rf 
         where 
            g = "g" << rf 

roundGrowB :: Rule (Ref Rational)
roundGrowB = buggyRule "round g incorrectly" f
   where 
      f rf 
         | (not.isPresent "g") rf = [] 
         | otherwise = concat 
            [("g", rndB k g)!:rf | k <- [1..3]] 
         where 
            g = "g" << rf 

calc_dx1xv :: Rule (Ref Rational)
calc_dx1xv = rule "calculate: dx1xv = xv - x1" f 
   where 
      f rf 
         | isPresent "dx1xv" rf = []
         | otherwise = ("dx1xv", xv - x1)|:rf 
         where x1 = "x1" << rf
               xv = "xv" << rf

dx1xv_Is_xv :: Rule (Ref Rational)
dx1xv_Is_xv = buggyRule "calculate: dx1xv = xv" f 
   where 
      f rf 
         | isPresent "dx1xv" rf = []
         | otherwise = ("dx1xv", xv)|:rf 
         where xv = "xv" << rf

calc_dx2xv :: Rule (Ref Rational)
calc_dx2xv = rule "calculate: dx2xv = xv - x2" f 
   where 
      f rf 
         | isPresent "dx2xv" rf = []
         | otherwise = ("dx2xv", xv - x2)|:rf 
         where x2 = "x2" << rf
               xv = "xv" << rf

dx2xv_Is_xv :: Rule (Ref Rational)
dx2xv_Is_xv = buggyRule "calculate: dx2xv = xv" f 
   where 
      f rf 
         | isPresent "dx2xv" rf = []
         | otherwise = ("dx2xv", xv)|:rf 
         where xv = "xv" << rf

inter1 :: Rule (Ref Rational)
inter1 = rule "calculate inter = y1*g^dx1xv" f 
   where 
      f rf 
         | isPresent "inter" rf = []
         | otherwise = ("inter", y1*g^.dx1xv)|:rf 
         where y1    = "y1"    << rf
               dx1xv = "dx1xv" << rf
               g     = "g"     << rf

inter2 :: Rule (Ref Rational)
inter2 = rule "calculate inter = y2*g^dx2xv" f 
   where 
      f rf 
         | isPresent "inter" rf = []
         | otherwise = ("inter", y2*g^.dx2xv)|:rf 
         where y2    = "y2"    << rf
               dx2xv = "dx2xv" << rf
               g     = "g"     << rf

baseIs_y1 :: Rule (Ref Rational)
baseIs_y1= rule "base = y1" f 
   where 
      f rf 
         | isPresent "bs" rf = []
         | otherwise = ("bs", y1)|:rf 
         where y1 = "y1" << rf

baseIs_y2 :: Rule (Ref Rational)
baseIs_y2= rule "base = y2" f 
   where 
      f rf 
         | isPresent "bs" rf = []
         | otherwise = ("bs", y2)|:rf 
         where y2 = "y2" << rf

baseIs_x1 :: Rule (Ref Rational)
baseIs_x1 = buggyRule "base = x1" f 
   where 
      f rf 
         | isPresent "bs" rf = []
         | otherwise = ("bs", x1)|:rf 
         where x1 = "x1" << rf

baseIs_x2 :: Rule (Ref Rational)
baseIs_x2 = buggyRule "base = x2" f 
   where 
      f rf 
         | isPresent "bs" rf = []
         | otherwise = ("bs", x2)|:rf 
         where x2 = "x2" << rf

baseIs_1 :: Rule (Ref Rational)
baseIs_1 = buggyRule "no base used" f 
   where 
      f rf 
         | isPresent "bs" rf = []
         | otherwise = ("bs", 1)|:rf 

newGrow1 :: Rule (Ref Rational)
newGrow1 = rule "ng = g^dx1xv" f 
   where 
      f rf 
         | isPresent "ng" rf = []
         | otherwise = ("ng", g^.dx1xv)|:rf 
         where g     = "g"     << rf
               dx1xv = "dx1xv" << rf

newGrow2 :: Rule (Ref Rational)
newGrow2 = rule "ng = g^dx2xv" f 
   where 
      f rf 
         | isPresent "ng" rf = []
         | otherwise = ("ng", g^.dx2xv)|:rf 
         where g     = "g"     << rf
               dx2xv = "dx2xv" << rf

newGrow1Mul :: Rule (Ref Rational)
newGrow1Mul = buggyRule "ng = g * dx1xv" f 
   where 
      f rf 
         | isPresent "ng" rf = []
         | otherwise = ("ng", g*dx1xv)|:rf 
         where g     = "g"     << rf
               dx1xv = "dx1xv" << rf

newGrow2Mul :: Rule (Ref Rational)
newGrow2Mul = buggyRule "ng = g * dx2xv" f 
   where 
      f rf 
         | isPresent "ng" rf = []
         | otherwise = ("ng", g*dx2xv)|:rf 
         where g     = "g"     << rf
               dx2xv = "dx2xv" << rf

newGrowIsg :: Rule (Ref Rational)
newGrowIsg = buggyRule "ng = g" f 
   where 
      f rf 
         | isPresent "ng" rf = []
         | otherwise = ("ng", g)|:rf 
         where g     = "g"     << rf

interB :: Rule (Ref Rational)
interB = rule "inter = bs * ng" f 
   where 
      f rf 
         | isPresent "inter" rf = []
         | otherwise = ("inter", bs * ng)|:rf 
         where bs = "bs" << rf
               ng = "ng" << rf

interAdd :: Rule (Ref Rational)
interAdd = buggyRule "inter = bs + ng" f 
   where 
      f rf 
         | isPresent "inter" rf = []
         | otherwise = ("inter", bs + ng)|:rf 
         where bs = "bs" << rf
               ng = "ng" << rf

finalRound :: Rule (Ref Rational)
finalRound = buggyRule "round final answer correctly" f
   where 
      f rf 
         | (not.isPresent "inter") rf = [] 
         | otherwise = concat 
            [("inter", rnd k inter)!:rf | k <- [1,2]] 
         where 
            inter = "inter" << rf

finalRound3 :: Rule (Ref Rational)
finalRound3 = rule "round final answer correctly 3" f
   where 
      f rf 
         | (not.isPresent "inter") rf = [] 
         | otherwise = ("inter", rnd 3 inter)!:rf  
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

finalAns :: Rule (Ref Rational)
finalAns = rule "get final asnwer" f 
   where 
      f rf = case "inter" <<  rf 
               of Nothing  -> []
                  Just ans -> [Map.singleton "inter" ans]

buggyRules :: [Rule (Ref Rational)]
buggyRules = 
   [ finalRound 
   , finalRoundB 
   , roundGrow
   , roundGrow3
   , roundGrowB
   , factorIsdy   
   , factorIsQuot
   , invertFactor
   , roundFactor
   , roundFactorB
   , dx_Is_x1
   , dx_Is_x2
   , growNoRecip
   , growNoExp
   , growDiv
   , invertGrow
   , dx1xv_Is_xv
   , dx2xv_Is_xv
   , baseIs_1
   , baseIs_y1
   , baseIs_y2
   , baseIs_x1
   , baseIs_x2
   , newGrowIsg
   , newGrow1Mul
   , newGrow2Mul
   , interAdd
   ]