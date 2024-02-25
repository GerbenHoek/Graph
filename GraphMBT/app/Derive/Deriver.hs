module Derive.Deriver where 
import FSMMaker.Rules
import FSMMaker.Trails
import FSMMaker.StrategyBas
import Data.List (sort)

splitter :: [a] -> [(a, [a])]
splitter xs = [takeOut $ splitAt n xs | n <- [0..l-1]]
   where takeOut (xs, y:ys) = (y, xs ++ ys)  
         l = length xs 

type Term = (Rational, Rational) -- a term represents a term in a polynomial cx^p <-> (c, p)
data Deriv = D { poly  :: [Term] -- poly represents a polynomial 
               , deriv :: [Term] -- terms of the polynomial can be differentiated individually, 
               }                 -- these terms are removed from poly and placed in deriv 
   deriving (Show, Eq, Ord)

insert :: Ord a => a -> [a] -> [a] -- Terms can be moved to deriv such that deriv is comparable
insert y [] = [y]
insert y (x:xs) = if y > x then x:insert y xs else y:x:xs 
--insert y xs = sort (y:xs)

liftToD :: Rule Term -> Rule Deriv 
liftToD (Rule b s f)  = Rule b s f'
   where 
      f' (D p d) = [D p' (insert fe d)  | 
         (e, p') <- splitter p, fe <- f e]  

derive :: Rule Term 
derive = rule "derive" f 
   where f (c, p) 
            | p == 0 = [(0,0)]
            | otherwise = [(c * p, p-1)]

forgetFactor :: Rule Term
forgetFactor = buggyRule "forget factor" f
   where f (c, p) = [(p, p-1)]

forgetCoef :: Rule Term
forgetCoef = buggyRule "forget coefficient" f
   where f (c, p) = [(1, p-1)]

powerPlus :: Rule Term 
powerPlus = buggyRule "add to power" f
   where f (c, p) = [(c * p, p+1)]

minusError :: Rule Term 
minusError = buggyRule "add to power" f
   where f (c, p) = [(-c * p, p-1)]

forgetDivTerm :: Rule Term 
forgetDivTerm = buggyRule "forget to differentiate a term" f
   where f (c, p) = [(c, p)]

deleteTerm :: Rule Term 
deleteTerm = buggyRule "delete a term" f
   where f (c, p) = [(0,0)]
            -- | p == 0 = []
            -- | otherwise = [(0,0)]

deleteX :: Rule Term 
deleteX = buggyRule "delete cx term" f
   where f (c, p)
            | p /= 1 = []
            | otherwise = [(0,0)]

leaveConst :: Rule Term 
leaveConst = buggyRule "leave the constant term" f
   where f (c, p)
            | p /= 0 = []
            | otherwise = [(c, p)]

leaveX :: Rule Term 
leaveX = buggyRule "leave the cx term" f
   where f (c, p)
            | p /= 1 = []
            | otherwise = [(c, p)]

rules :: [Rule Term]
rules = 
   [ derive
   , forgetFactor
   , forgetCoef
   , powerPlus
   , minusError
   , forgetDivTerm
   , deleteTerm
   , deleteX
   , leaveConst
   , leaveX
   ]

divStrat :: Strat Deriv 
divStrat = (repeatS $ Lbl "derive" $ foldr (<|>) Fail $ map (R . liftToD) rules) 

deriv1 =          D [(3,7), (-2,3), (1,1), (7,0), (22,4)] []

deriv2 = D [] (sort [(21,6), (6,2), (0,0),(7 ,0), (0,0)])





