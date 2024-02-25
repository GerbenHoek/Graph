module Logic.LogicStrat where
import FSMMaker.StrategyBas
import Logic.LogicRules
import FSMMaker.Trails 
import FSMMaker.Rules
import Logic.Logic

testi1 :: Logic Int
testi1 = 
   (Not (Var 6) .<=>. Not T) .&&. ((Var 1 .&&. Not (Var 2)) 
   .||. Not (Var 2 .<=>. Var 4)) 
  

testi2 :: Logic Int
testi2 = dnf 10 $
   F .&&. ((Var 1 .&&. Not (Var 2)) 
   .||. Not (Var 2 .<=>. Var 4)) 

testi3 :: Logic Int
testi3 = 
   (Var 6) :&&: ((Var 1 :&&: Not (Var 2)) :||: 
      Not ((Var 2 :&&: Var 4) :||: (Not (Var 2) :&&: Not (Var 4))))

testi4 :: Logic Int
testi4 = dnf 10 $
   (Var 6) :&&: ((Var 1 :&&: Not (Var 2)) :||: 
      (Not (Var 2 :&&: Var 4) :&&: Not (Not (Var 2) :&&: Not (Var 4))))

testi5 :: Logic Int
testi5 = dnf 10
   (Var 6) :&&: ((Var 1 :&&: Not (Var 2)) :||: 
      ((Not (Var 2) :&&: Not (Var 4)) :&&: Not (Not (Var 2) :&&: Not (Var 4))))

smw :: Strat (Logic a) -> Strat (Logic a) 
smw = somewhere contextL 

constantsAnd :: Strat (Logic a) 
constantsAnd = R andTrue <|> R andFalse <|> R andTrueB <|> R andFalseB

constantsOr :: Strat (Logic a) 
constantsOr = R orTrue <|> R orFalse <|> R orTrueB <|> R orFalseB

constantsNot :: Strat (Logic a) 
constantsNot = R notTrue <|> R notFalse

constantsImpl :: Strat (Logic a) 
constantsImpl = R implTrue <|> R implFalse <|> R trueImpl <|> R falseImpl <|>
                R implTrueB <|> R implFalseB <|> R trueImplB <|> R falseImplB

constantsEq :: Strat (Logic a) 
constantsEq = R trueEquiv <|> R falseEquiv <|> R trueEquivB <|> R falseEquivB  

switch :: Strat (Logic a) 
switch = option $ R switchAnd <|> R switchOr <|> R switchEq

reduceconst :: Ord a => Strat (Logic a) 
reduceconst = repeatS $ smw $
   constantsAnd <|> 
   constantsOr <|> 
   constantsNot <|> 
   constantsImpl <|>
   constantsEq

removeArrows :: Ord a => Strat (Logic a) 
removeArrows = repeatS $ smw (R implDef <|> R equivDef <|> R equivDefB)

deMorgans' :: Strat (Logic a)
deMorgans' = R deMorganAnd <|> R deMorganOr <|>  
             R deMorganAndB1 <|> R deMorganOrB1 <|> 
             R deMorganAndB2 <|> R deMorganOrB2

nots :: Strat (Logic a)            
nots = R notNot <|> R notNotB -- <|> R notB

deMorgans :: Ord a => Strat (Logic a)
deMorgans = repeatS (smw nots) .*. 
            repeatS (smw deMorgans')  .*. 
            repeatS (smw nots)


distribution :: Ord a => Strat (Logic a)
distribution = repeatS $ smw (R andOverOr) 

tautsAndConsts' :: Ord a => Strat (Logic a)
tautsAndConsts' = 
   R implTaut <|>
   R equivTaut <|>
   R orTaut <|>
   R equivContr <|>
   R andContr -- <|>
   --R andSame <|>
   --R orSame

tautsAndConsts :: Ord a => Strat (Logic a)
tautsAndConsts = 
   repeatS (smw tautsAndConsts') .*.
   reduceconst
   
toDNF :: Strat (Logic Int)
toDNF = tautsAndConsts .*.
        reduceconst .*. 
        tautsAndConsts .*.
        removeArrows .*.
        tautsAndConsts .*. 
        deMorgans .*.
        tautsAndConsts .*.
        distribution .*.
        tautsAndConsts  .*.
        R (stDNF 10)
