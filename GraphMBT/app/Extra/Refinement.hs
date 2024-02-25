module Extra.Refinement where 
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map

type Ref a = Map String a

instance Num a => Num (Maybe a) where 
    a + b = (+) <$> a <*> b
    a - b = (-) <$> a <*> b
    a * b = (*) <$> a <*> b
    fromInteger a = Just $ fromInteger a
    abs a = abs <$> a
    signum a = signum <$> a

instance Fractional a => Fractional (Maybe a) where 
    a / b = (/) <$> a <*> b
    fromRational r = Just $ fromRational r

isPresent :: String -> Ref a -> Bool
isPresent = Map.member 


(|:) :: (String, Maybe a) -> Ref a-> [Ref a]
(str, mn) |: rf =
   case mn
     of Nothing -> []
        Just n  -> [Map.insert str n rf]

--replace item 
(!:) :: (String, Maybe a) -> Ref a -> [Ref a]
(str, mn) !: rf = 
   case mn
     of Nothing -> []
        Just n  -> [Map.insert str n rf]

-- take item out if the refinement
(<<) :: String -> Ref a -> Maybe a
str << rf = Map.lookup str rf