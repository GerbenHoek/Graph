module SimplinStrat where 
import Trails 
import StrategyBas
import Rules
import Expressions hiding ((:*:))
import SimpLin 
import Utils

expr1 = 4 - 9 * (9 + 7 * Var "x") - Var "x"
expr4 = 2 - 8 * (9 + 5 * Var "x") - Var "x"


expr2 = evaluate $ (-45) + 6 * Var "x"
expr3 = evaluate $ (-85) + 6 * Var "x"

smw :: Strat Expr -> Strat Expr
smw = fmap (liftToContext contextE) 

eval :: Strat Expr
eval = R evalAll

addFirst2 :: Strat Expr
addFirst2 = 
   Many (smw $ R addForMul <|> R addForMulR) .*. 
   (repeatS $ smw $ Lbl "evalAdd" $ R evalAdd)

addFirst :: Strat Expr
addFirst = 
   Many (smw $ R addForMulN) .*. 
   (repeatS $ smw $ Lbl "evalAdd" $ R evalAdd)

distribution :: Strat Expr
distribution = repeatS $ smw $ Lbl "distribution" $
   R forgetfactor1 <|> 
   R forgetfactor2 <|>
   R removeBrackets

done :: Strat Expr
done = filterS finalFormS "done"

addTerms' :: Strat Expr
addTerms' = 
   smw $ 
   Lbl "addTerms" $
   R evalAdd <|> 
   R factorX <|>
   R addNumX <|>
   R vanishX1 <|>
   R vanishX2 <|>
   R loseTerm

addTerms :: Strat Expr
addTerms = 
   try (addTerms') .*. 
   Many (smw (R switchTerms) .*. addTerms'.*. eval) .*.
   option (smw $ R evalAdd)

minusS :: Strat Expr
minusS = option (smw $ R loseMinus <|> R addMinus)

simplify :: Strat Expr
simplify = 
   eval .*.
   minusS .*.
   addFirst2 .*. 
   eval .*.
   minusS .*.
   distribution .*.
   eval  .*.
   minusS  .*.
   addTerms .*.
   --done  .*. 
   eval .*.
   minusS -- .*. 
   -- finalS

finalS = R $ rule "finalS" toFinalFormS 

toFinalFormS :: Expr -> [Expr]
toFinalFormS expr = 
  case apply 2 expr 
    of [a1, a2] -> [Number (a2 - a1) * Var "x" + Number a1] 
       []       -> []

finalFormS :: Expr -> Bool
finalFormS expr = 
  case toFinalFormS expr 
    of [expr'] -> evaluate expr' == evaluate expr 
       _       -> False

