module SimpLin where
import Data.Generics.Uniplate.Direct
import Expressions
import Data.Maybe (maybeToList, listToMaybe)
import Data.List (transpose)
import Utils
import Rules 
import Data.Set (Set)

tl :: Maybe a -> [a]
tl = maybeToList

evalAdd :: Rule Expr
evalAdd = rule "add two terms" (tl.f)
 where 
  f expr = 
     case expr
       of Number n1 :+: (Number n2 :+: expr') -> Just $ Number (n1 + n2) + expr'
          Number n1 :+: Number n2 -> Just $ Number (n1 + n2)
          _ -> Nothing

evalMul :: Rule Expr
evalMul = rule "multiply two factors" (tl.f)
 where
  f expr = 
     case expr
       of Number n1 :*: (Number n2 :*: expr') -> Just $ Number (n1 * n2) * expr'
          Number n1 :*: Number n2 -> Just $ Number (n1 * n2)
          _ -> Nothing

evalSub :: Rule Expr
evalSub = rule "evaluate subtraction" (tl.f)
 where 
  f expr =
     case expr
       of expr1 :-: expr2 -> Just $ expr1 + Negate expr2
          _ -> Nothing

evalDiv :: Rule Expr
evalDiv = rule "evaluate division" (tl.f)
 where 
  f expr = 
    case expr
      of expr1 :/: expr2 -> Just $ expr1 * Recip expr2
         _ -> Nothing

evalNeg :: Rule Expr
evalNeg = rule "evaluate negation" (tl.f)
 where 
  f expr = 
     case expr
       of Negate expr' -> Just $ Number (-1) * expr'
          _ -> Nothing

evalRec :: Rule Expr
evalRec = rule "evaluate reciproke" (tl.f)
 where 
  f expr = 
     case expr
       of Recip (Number n) 
           | n /= 0 -> Just $ Number (1 / n)
          _ -> Nothing

countX :: Rule Expr
countX = rule "give x factor 1" (tl.f)
 where 
  f expr =
       case expr
         of Var "x" -> Just $ Number 1 * Var "x"
            _ -> Nothing

multiOnes :: Rule Expr
multiOnes = rule "multiply ones" (tl.f)
   where 
      f expr = 
           case expr 
             of Number 1 :*: Number 1 -> Just $ Number 1 
                _ -> Nothing

construct :: Rule Expr
construct = rule "use smart constructors" (tl.f)
 where 
  f expr = 
     case expr
       of expr1 :+: expr2 -> Just $ expr1 + expr2
          expr1 :*: expr2 -> Just $ expr1 * expr2
          expr1 :-: expr2 -> Just $ expr1 - expr2
          expr1 :/: expr2 -> Just $ expr1 / expr2
          _ -> Nothing

evaluate :: Expr -> Expr
evaluate = getFix (transform f) 
   where
      g r = always (listToMaybe . (applyRule r))
      f = --g evalAdd .
          g evalMul .
          g evalDiv .
          g evalNeg .
          g evalRec .
          g evalSub . 
          g countX .
          g construct 

evalAll :: Rule Expr
evalAll = rule "evaluate" (tl.f)
   where f = Just . evaluate

switchTerms :: Rule Expr
switchTerms = rule "switch two terms" (tl.f)
 where 
  f expr = 
     case expr
       of expr1 :+: (expr2 :+: expr3) -> Just $ expr2 + expr1 + expr3
          expr1 :+: expr2 -> Just $ expr2 + expr1 
          _ -> Nothing

switchFactors :: Rule Expr
switchFactors = rule "switch two factors" (tl.f)
 where 
  f expr = 
     case expr
       of expr1 :*: (expr2 :*: expr3) -> Just $ expr2 * expr1 * expr3
          expr1 :*: expr2 -> Just $ expr2 * expr1 
          _ -> Nothing

removeBrackets :: Rule Expr
removeBrackets = rule "remove brackets" (tl.f)
 where 
  f expr = 
     case expr
       of expr1 :*: (expr2 :+: expr3) -> Just $ expr1 * expr2 + expr1 * expr3
          _ -> Nothing

factor :: Rule Expr
factor = rule "take a common factor out" (tl.f)
 where 
  f expr = 
      case expr
        of (expr1 :*: expr2) :+: ((expr3 :*: expr4) :+: expr')
            | expr1 == expr3 -> Just $ expr1 * (expr2 + expr4) + expr'
            | otherwise      -> Nothing  
           (expr1 :*: expr2) :+: (expr3 :*: expr4) 
            | expr1 == expr3 -> Just $ expr1 * (expr2 + expr4)
            | otherwise      -> Nothing           
           _ -> Nothing

factorX :: Rule Expr
factorX = rule "take terms with x's together" (tl.f)
 where
  f expr =
      case expr
        of (expr1 :*: Var "x") :+: ((expr2 :*: Var "x") :+: expr')
            -> Just $ (expr1 + expr2) * Var "x" + expr'
           (expr1 :*: Var "x") :+: (expr2 :*: Var "x") 
            -> Just $ (expr1 + expr2) * Var "x"
           _ -> Nothing

forgetfactor1 :: Rule Expr
forgetfactor1 = buggyRule "remove brackets and forget the second term" (tl.f)
 where 
  f expr = 
     case expr
       of expr1 :*: (expr2 :+: expr3) -> Just $ expr1 * expr2 + expr3
          _ -> Nothing

forgetfactor2 :: Rule Expr
forgetfactor2 = buggyRule "remove brackets and forget the first term" (tl.f)
 where 
  f expr = 
     case expr
       of expr1 :*: (expr2 :+: expr3) -> Just $ expr2 + expr1 * expr3
          _ -> Nothing

vanishX1 :: Rule Expr
vanishX1 = buggyRule "delete the x when subtracting only x" (tl.f)
 where 
  f expr = 
     case expr
       of (Number n :*: Var "x") :+: ((Number (-1) :*: Var "x") :+: expr') 
           -> Just $ Number n + expr'
          (Number n :*: Var "x") :+: (Number (-1) :*: Var "x") 
           -> Just $ Number n
          _ -> Nothing

vanishX2 :: Rule Expr
vanishX2 = buggyRule "delete the x when subtracting" (tl.f)
 where 
  f expr = 
     case expr
       of (Number n1 :*: Var "x") :+: ((Number n2 :*: Var "x") :+: expr') 
            |  n2 <0 -> Just $ Number (n1+n2) + expr'
          (Number n1 :*: Var "x") :+: (Number n2 :*: Var "x") 
            | n2 < 0 -> Just $ Number (n1+n2)
          _ -> Nothing

addNumX :: Rule Expr
addNumX = buggyRule "add a constant term to a variable term" (tl.f)
 where 
  f expr = 
     case expr
       of (Number n1 :*: Var "x") :+: (Number n2 :+: expr')
            -> Just $ Number (n1 + n2) * Var "x" + expr'
          (Number n1 :*: Var "x") :+: Number n2 
            -> Just $ Number (n1 + n2) * Var "x"
          _ -> Nothing

addForMul :: Rule Expr
addForMul = buggyRule "addition before multiplication" (tl.f)
 where
  f expr = 
     case expr
       of expr1 :+: ((expr2 :*: expr3) :+: expr')
            -> Just $ (expr1 + expr2) * expr3 + expr'
          expr1 :+: (expr2 :*: expr3)
            -> Just $ (expr1 + expr2) * expr3
          _ -> Nothing

addForMulR :: Rule Expr
addForMulR = buggyRule "addition before multiplication R" (tl.f)
 where
  f expr = 
     case expr
       of (expr2 :*: expr3) :+: (expr1 :+: expr')
            -> Just $ expr2 * (expr3 + expr1) + expr'
          (expr2 :*: expr3) :+: expr1
            -> Just $ expr2 * (expr3 + expr1)
          _ -> Nothing


addForMulN :: Rule Expr
addForMulN = buggyRule "addition before multiplication with numbers" (tl.f)
 where
  f expr = 
     case expr
       of Number n1 :+: ((Number n2 :*: expr3) :+: expr')
            -> Just $ (Number n1 + Number n2) * expr3 + expr'
          Number n1 :+: (Number n2 :*: expr3)
            -> Just $ (Number n1 + Number n2) * expr3
          _ -> Nothing

loseMinus :: Rule Expr
loseMinus = buggyRule "lose minus" (tl.f)
   where 
      f expr =
         case expr 
           of Number n | n < 0 
                 -> Just $ Number (- n)
              Number n :+: expr | n < 0 
                 -> Just $ Number (- n) + expr
              Number n :*: expr | n < 0 
                 -> Just $ Number (- n) * expr
              _                -> Nothing

addMinus :: Rule Expr
addMinus = buggyRule "add minus" (tl.f)
   where 
      f expr = 
         case expr 
           of Number n | n > 0 
                 -> Just $ Number (- n)
              Number n :+: expr | n > 0 
                 -> Just $ Number (- n) + expr
              Number n :*: expr | n > 0 
                 -> Just $ Number (- n) * expr
              _                -> Nothing

loseTerm :: Rule Expr
loseTerm = buggyRule "lose term" (tl.f)
   where 
      f expr =
         case expr 
           of (Number n :*: Var "x") :+: expr  -> Just expr
              Number n :+: expr -> Just expr
              _                 -> Nothing


doNothing :: Rule Expr
doNothing = rule "do nothing" (tl.f)
 where f expr = Just expr 

rules :: [Rule Expr]
rules = 
 [evalAdd, 
  switchTerms,
  switchFactors,
  removeBrackets,
  factor,
  factorX,
  evalAll,
  multiOnes]

buggyRules :: [Rule Expr]
buggyRules = 
 [forgetfactor1,
  forgetfactor2,
  vanishX1,
  vanishX2,
  addNumX,
  addForMul,
  addForMulR,
  addForMulN,
  loseMinus,
  addMinus,
  loseTerm]

evalRule :: Rule Expr -> Rule Expr
evalRule (Rule b s f) = Rule b s (tl.f')
 where f' expr =
        case f expr 
          of [expr'] -> Just $ evaluate expr'
             _          -> Nothing  

allRules :: [Rule Expr]
allRules = 
    liftRules contexts (rules ++ buggyRules)
   
apply :: Int -> Expr -> [Rational]
apply k expr = 
 case expr 
   of Number n -> take k . repeat $ n 
      Var _ -> [0..k'] 
         where k' = toRational k
      Negate expr -> zipWith (*) (take k . repeat $ (-1)) (apply k expr)
      Recip expr 
         | 0 `elem` (apply k expr) -> []
         | otherwise -> zipWith (/)  (take k . repeat $ (1)) (apply k expr) 
      expr1 :+: expr2 -> zipWith (+) (apply k expr1) (apply k expr2)
      expr1 :*: expr2 -> zipWith (*) (apply k expr1) (apply k expr2)
      expr1 :-: expr2 -> zipWith (-) (apply k expr1) (apply k expr2)
      expr1 :/: expr2 
         | 0 `elem` (apply k expr2) -> []
         | otherwise -> zipWith (/) (apply k expr1) (apply k expr2)





