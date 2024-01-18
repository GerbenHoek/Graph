module Rules where 
import Data.Maybe

data MaybeRule a = MRule Bool String (a -> Maybe a)

data Rule a = 
   Rule {isBuggy :: Bool, 
         ruleName :: String, 
         applyRule :: a -> [a]}

instance Eq (Rule a) where 
   r1 == r2 = ruleName r1 == ruleName r2

instance Ord (Rule a) where 
   compare r1 r2 = compare (ruleName r1) (ruleName r2)

instance Show (Rule a) where 
   show r = ruleName r




rule :: String -> (a -> [a]) -> Rule a
rule = Rule False 

buggyRule :: String -> (a -> [a]) -> Rule a
buggyRule = Rule True 

label :: [Rule a] -> String -> Rule a 
label rs s = (fromMaybe err . lookup s) [(ruleName r, r) | r <- rs]
   where err = error $ s ++ " not among rulenames"


                  
type Context a = a -> [(a, a -> a)] 

liftToContext :: Context a -> Rule a -> Rule a
liftToContext ct r = 
   Rule (isBuggy r) ("SW " ++ ruleName r) f
      where f a = [g a'' | (a', g) <- ct a, a'' <- applyRule r a']

topDown' :: Context a -> Rule a -> Rule a 
topDown' ct (Rule b n f) = 
   Rule b ("TD " ++ n) f'
      where 
         f' a = 
            case applyRule (liftToContext ct (Rule b n f)) a
            of [] -> [] 
               as -> [head as]

bottomUp' :: Context a -> Rule a -> Rule a 
bottomUp' ct (Rule b n f) = 
   Rule b ("BU " ++ n) f'
      where 
         f' a = 
            case applyRule (liftToContext ct (Rule b n f)) a
            of [] -> [] 
               as -> [last as]

liftRules :: Context a -> [Rule a] -> [Rule a]
liftRules ct rs = 
   rs ++ 
   map (liftToContext ct) rs ++
   map (topDown' ct) rs ++
   map (bottomUp' ct) rs



