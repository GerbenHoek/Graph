module StrategyBas where
import Trails
import Rules
import Data.List (iterate)

type Strat a = Trail (Rule a) 

applyAll :: Strat a -> a -> [a]
applyAll s a = [c | (r, s') <- firsts s, 
                b <- applyRule r a, c <- applyAll s' b] ++
                if empty s then [a] else []

somewhere :: Context a -> Strat a -> Strat a
somewhere ct = fmap (liftToContext ct)  

filterS :: (a -> Bool) -> String -> Strat a 
filterS p str = R (rule str f)
   where 
      f a
       | p a = [a]
       | otherwise = []  

notS :: Ord a => Strat a -> Strat a
notS s = R $ rule (str) f
    where
       f a = if null $ applyAll s a 
             then [a] else []
       str = case s of 
             Lbl str' t -> "not " ++ str'
             _          -> "not " ++ show s
 
fix :: (a -> a) -> a
fix f = f (fix f)

manyN :: Int -> Strat a -> Strat a
manyN n s = (last . (take n) . iterate (\x -> Succeed <|> (s .*. x))) s

repeatS :: Ord a => Strat a -> Strat a
repeatS s = Many s .*. (notS s)

repeatN :: Ord a => Int -> Strat a -> Strat a
repeatN n s = (manyN n s) .*. (notS s)

many1S :: Strat a -> Strat a
many1S s = Many s .*. s 

repeat1S :: Ord a => Strat a -> Strat a
repeat1S s = many1S s .*. (notS s)

option :: Strat a -> Strat a  
option s = s <|> Succeed 

try :: Ord a => Strat a -> Strat a
try s = s <|> notS s

-- left bias 
(|>) :: Ord a => Strat a -> Strat a -> Strat a
s |> t = s <|> ((notS s) .*. t)






