module NubSub where
import qualified Data.Set as Set
import Data.List as List

merge :: Ord a => [a] -> [a] -> [a]
merge [] x = x
merge x [] = x
merge (x:xs) (y:ys) | y < x     = y : merge (x:xs) ys
merge (x:xs) (y:ys) | otherwise = x : merge xs (y:ys)

newtype L a = L {getL :: (Int, [a])}

(|:) :: a -> L a -> L a
a |: (L (n, as)) = L (n + 1, a:as) 

emptyL :: L a
emptyL = L (0,[])

instance Show a => Show (L a) where
   show = show . snd . getL 

instance Eq (L a) where
   l1 == l2 = (fst $ getL l1) == (fst $ getL l2)

instance Ord (L a) where
   compare l1 l2 = compare (fst $ getL l1) (fst $ getL l2)

isSubSet :: Ord a => L a -> L a -> Bool
isSubSet l1 l2 = -- all (`elem` bs) as
  Set.isSubsetOf 
     (Set.fromList . snd . getL $ l1) (Set.fromList . snd . getL $ l2)

nubSub :: Ord a => [L a] -> [L a] -> [L a]  
nubSub ls ls' = List.nubBy isSubSet $ merge ls ls'