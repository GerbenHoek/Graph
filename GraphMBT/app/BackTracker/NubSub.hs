module BackTracker.NubSub where
import qualified Data.Set as Set
import Data.List as List

merge :: Ord a => [a] -> [a] -> [a]
merge [] x = x
merge x [] = x
merge (x:xs) (y:ys) | y < x     = y : merge (x:xs) ys
                    | otherwise = x : merge xs (y:ys)

data L a = L {size :: Int, list :: [a]}
   deriving (Eq, Ord)

(|:) :: a -> L a -> L a
a |: (L n as) = L (n + 1) (a:as) 

emptyL :: L a
emptyL = L 0 []

instance Show a => Show (L a) where
   show = show . list


a = [1,2,3]
b = take 12 $ cycle a 

isCycle :: Eq a => [a] -> [a] -> Bool
isCycle as bs = isCycle' as as bs
   where 
      isCycle' _ [] [] = True
      isCycle' _ _  [] = False
      isCycle' xs [] ys = 
         isCycle' xs xs ys
      isCycle' zs (x:xs) (y:ys) = 
         x == y && isCycle' zs xs ys 

isCycleL :: Eq a => L a -> L a -> Bool 
isCycleL la lb = (list la) `isCycle` (list lb)  

areCyclesL :: Ord a => [L a] -> [L a] -> Bool
areCyclesL [] [] = True
areCyclesL _  [] = False
areCyclesL [] _  = False
areCyclesL (a:la) (b:lb) = isCycleL a b && areCyclesL la lb 

isSubSet :: Ord a => L a -> L a -> Bool
isSubSet l1 l2 = -- all (`elem` bs) as
  Set.isSubsetOf 
     (Set.fromList . list $ l1) (Set.fromList . list $ l2)

nubSub :: Ord a => [L a] -> [L a] -> [L a]  
nubSub ls ls' = List.nubBy isSubSet $ merge ls ls'