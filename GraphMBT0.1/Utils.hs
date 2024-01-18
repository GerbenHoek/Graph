module Utils where
import qualified Data.List as L
import qualified Data.Set as S
import System.Random
import Numeric
import Data.Maybe (fromMaybe)

rounder :: Int -> Rational -> Rational
rounder k = (/10^k) . round' . (* 10^k) 
   where 
      round' b = (signum b) * round'' (abs b)  
      round'' c  
         | c - tr floor c >= 1 / 2 = tr ceiling c
         | otherwise = tr floor c
      tr f = toRational . f

floatToR :: String -> Rational 
floatToR  str =
   case (readSigned readFloat) str of
      ((n, []):_) -> n
      _           -> error "Invalid number"

--getR str = 
--   case (readSigned readFloat) str of
--      ((n, []):_) -> n
--      _           -> 

setEq :: Ord a => [a] -> [a] -> Bool
setEq as bs = S.fromList as == S.fromList bs

isSubSet :: Ord a => [a] -> [a] -> Bool
isSubSet as bs = -- all (`elem` bs) as
  S.isSubsetOf (S.fromList as) (S.fromList bs)

setDifference :: Ord a => [a] -> [a] -> [a]
setDifference a = 
   S.toList . S.difference (S.fromList a) . S.fromList 

setNub :: Ord a => [a] -> [a]
setNub = S.toAscList . S.fromList

nubBy :: (a -> a -> Bool) -> [a] -> [a]
nubBy = L.nubBy

sorter :: (Ord b) => (a -> b) -> [a] -> [a]
sorter f = L.sortBy (\x y -> compare (f x) (f y))

grouper :: (Ord b) => (a -> b) -> [a] -> [[a]]
grouper f xs = L.groupBy (\x y -> f x == f y) $ sorter f xs

nubSub :: Ord a => [[a]] -> [[a]]
nubSub = L.nubBy isSubSet . sorter length . setNub

takeWhileP1 :: (a -> Bool) -> [a] -> [a]
takeWhileP1 p = foldr (\x ys -> if p x then x:ys else [x]) []

iterWhile :: (a -> Bool) -> (a -> a) -> a -> a 
iterWhile b f = last . takeWhile b . L.iterate f 

iterWhileP1 :: (a -> Bool) -> (a -> a) -> a -> a 
iterWhileP1 b f = f . iterWhile b f

iterN :: Int -> (a -> a) -> a -> a
iterN n f = last . take n . L.iterate f

getFix :: Eq a => (a -> a) -> a -> a 
getFix f = stable . L.iterate f
   where 
      stable [x] = 
         error "getFix: function does not stabelize"
      stable (x:y:xs) = 
         if x == y then x else stable xs

getFix' :: Eq a => (a -> a) -> a -> a
getFix' f a 
   | new == a  = a
   | otherwise = getFix f new
      where new = f a

takeUnique :: Ord a => Int -> [a] -> [a]
takeUnique k xs = 
 (last . takeWhile (\a -> length a <= k) .
 map (\k' -> setNub (take k' xs))) [1..]

index :: (Show a, Eq a) => a -> [a] -> Int
index rl = fromMaybe err . L.elemIndex rl 
   where 
      err = error $ show rl ++ " not in list"

rms :: Num r => Int -> Int -> Int -> [r]
rms r k1 k2 = map fromIntegral (randomRs (k1,k2) (mkStdGen r) :: [Int])

rmL :: Num a => Int -> Int -> Int -> [[a]]
rmL n k1 k2 = map (\r -> take n (rms r k1 k2)) [0..] 

notIn :: Eq a => [a] -> [a] -> Bool
notIn as = null . (L.intersect as) 
