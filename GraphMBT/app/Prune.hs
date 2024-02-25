module Prune where
import FSMMaker.Rules
import FSMMaker.FSM
import Data.Maybe

newtype Ct a = C (a, Int)
   deriving (Ord, Eq, Show)
--instance Eq a => Eq (Ct a) where
--   (C c1) == (C c2) = (fst c1) == (fst c2)

--instance Ord a => Ord (Ct a) where
--   compare (C c1) (C c2) = compare (fst c1) (fst c2)

toCt :: a -> Int -> Ct a
toCt a n = C (a, n)

getPoint :: Ct a -> a
getPoint (C (a, n)) = a

pruneRule :: Int -> (Rule a -> Bool) -> Rule a -> Rule (Ct a)
pruneRule n b r@(Rule ib s f) = Rule ib s f'
   where 
      f' (C (a, k)) 
         | k >= n    = []
         | b r       = zipWith toCt (f a) (repeat (k+1))  
         | otherwise = zipWith toCt (f a) (repeat k)

pruneBuggy :: Int -> Rule a -> Rule (Ct a)
pruneBuggy n = pruneRule n isBuggy

pruneFSM :: Int -> (Rule a -> Bool) 
   -> FSM s (Rule a) -> FSM s (Rule (Ct a))
pruneFSM n b fsm = fmap (pruneRule n b) fsm

pruneBFSM :: Int -> FSM s (Rule a) 
   -> FSM s (Rule (Ct a))
pruneBFSM n = pruneFSM n isBuggy