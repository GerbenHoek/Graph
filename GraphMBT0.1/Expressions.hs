module Expressions where
{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
import GHC.Generics (Generic)
import Control.DeepSeq
import Data.Ratio 
import Data.Typeable
import Data.Data
import Data.Maybe
import Data.Generics.Uniplate.Direct

data Expr = -- Num
            Expr :+: Expr
          | Expr :*: Expr
          | Expr :-: Expr
          | Expr :/: Expr
          | Negate Expr
          | Recip Expr
          | Abs Expr
          | Sign Expr
          | Number Rational 
          | Var String
   deriving 
      (Eq, Ord, Data, Typeable, Generic, Read, Show)
instance NFData Expr 

showEx :: Expr -> String
showEx (expr1 :+: Negate expr2) = showEx (expr1 - expr2)
showEx (expr1 :+: (Negate expr2 :+: expr3)) = showEx (expr1 - expr2 + expr3)  
showEx (expr1 :+: (Negate expr2 :*: expr3)) = showEx (expr1 - (expr2 * expr3))  
showEx (expr1 :+: (Negate expr2 :/: expr3)) = showEx (expr1 - (expr2 / expr3))
showEx ((Number r :*: Var str) :+: expr) = 
      brack (Number r * Var str) ++ " + " ++ showEx expr 
showEx (Number r :*: Var str) = brack (Number r :*: Var str) 
showEx (expr1 :+: expr2) = showEx expr1 ++ " + " ++ showEx expr2
showEx (expr1 :-: expr2) = showEx expr1 ++ " - " ++ brack expr2
showEx (expr1 :*: expr2) = brack expr1 ++ " * " ++ brack expr2
showEx (expr1 :/: expr2) = brack expr1 ++ " / " ++ brack expr2
showEx (Number r) 
      | denominator r == 1 = show (numerator r)
      | otherwise = show (numerator r) ++ " / " ++ show (denominator r)
showEx (Var str)  = str
showEx (Negate expr) = "-" ++ brack expr  

brack :: Expr -> String
brack expr = 
   case expr 
     of Number r :*: Var str -> showEx (Number r) ++ "*" ++ showEx (Var str)
        Number r -> showEx (Number r)
        Negate (Number r) -> "-" ++ showEx (Number r)
        Var str  -> str
        _        -> "(" ++ showEx expr ++ ")" 


instance Num Expr where
   expr + Number 0 = expr
   Number 0 + expr = expr
   (expr1 :+: expr2) + expr3 = expr1 + (expr2 + expr3)
   expr1 + expr2 = expr1 :+: expr2
   expr * Number 0 = Number 0
   Number 0 * expr = Number 0
   (expr1 :*: expr2) * expr3 = expr1 * (expr2 * expr3)
   expr1 * expr2 = expr1 :*: expr2
   expr - Number 0 = expr
   Number 0 - expr = Negate expr
   expr1 - expr2 = expr1 :-: expr2
   fromInteger n
      | n < 0     = negate $ Number $ fromIntegral $ abs n
      | otherwise = Number $ fromIntegral n
   negate = Negate
   abs    = Abs
   signum = Sign

instance Uniplate Expr where
    uniplate (Number r)       = plate Number |- r
    uniplate (Var str)        = plate Var    |- str 
    uniplate (Sign expr)      = plate Sign   |* expr
    uniplate (Abs expr)       = plate Abs    |* expr
    uniplate (Recip expr)     = plate Recip  |* expr
    uniplate (Negate expr)    = plate Negate |* expr
    uniplate (expr1 :+: expr2) = plate (:+:) |* expr1 |* expr2
    uniplate (expr1 :*: expr2) = plate (:*:) |* expr1 |* expr2
    uniplate (expr1 :-: expr2) = plate (:-:) |* expr1 |* expr2
    uniplate (expr1 :/: expr2) = plate (:/:) |* expr1 |* expr2

instance Fractional Expr where
 Number 0 / expr = Number 0
 expr1 / expr2 = expr1 :/:expr2
 fromRational r
      | denominator r == 1 =
           fromIntegral (numerator r)
      | numerator r < 0 =
           Negate (fromIntegral (abs (numerator r)) :/: fromIntegral (denominator r))
      | otherwise =
           fromIntegral (numerator r) :/: fromIntegral (denominator r)


contextE :: Uniplate on => on -> [(on, on -> on)]
contextE = contexts 


always :: (Expr -> Maybe Expr) -> Expr -> Expr
always r x = fromMaybe x (r x)

sometimes :: (Expr -> Expr) -> Expr -> Maybe Expr
sometimes r x 
 | r x == x = Nothing
 | otherwise = Just $ r x

