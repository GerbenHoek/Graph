{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
module Logic.Logic where
import GHC.Generics 
import Control.Monad
import Data.Foldable 
import Utils
import Data.Maybe (fromJust)
import Data.Data
import Data.Generics.Uniplate.Direct

data Logic a =
      T | F
    | Logic a :||: Logic a 
    | Logic a :&&: Logic a 
    | Logic a :=>: Logic a
    | Logic a :<=>: Logic a
    | Not (Logic a)
    | Var a
   deriving (Eq, Ord, Show, Data, Typeable)


(.||.), (.&&.), (.=>.), (.<=>.) :: Logic a -> Logic a -> Logic a
(a :||: b) .||. c = a .||. (b .||. c)
a .||. b  = a :||: b
(a :&&: b) .&&. c = a .&&. (b .&&. c)
a .&&. b  = a :&&: b
(a :=>: b) .=>. c = a .=>. (b .=>. c)
a .=>. b  = a :=>: b
(a :<=>: b) .<=>. c = a .<=>. (b .<=>. c)
a .<=>. b  = a :<=>: b

instance Uniplate (Logic a) where
    uniplate T             = plate T
    uniplate F             = plate F
    uniplate (Var a)       = plate Var |- a
    uniplate (Not l)       = plate Not |* l
    uniplate (l1 :||: l2)  = plate (:||:) |* l1 |* l2
    uniplate (l1 :&&: l2)  = plate (:&&:) |* l1 |* l2
    uniplate (l1 :=>: l2)  = plate (:=>:) |* l1 |* l2
    uniplate (l1 :<=>: l2) = plate (:<=>:) |* l1 |* l2    

contextL :: Uniplate on => on -> [(on, on -> on)]
contextL = contexts 

childL :: Uniplate on => on -> [on]
childL = children

instance Functor Logic where
   fmap _ T = T
   fmap _ F = F 
   fmap g (Var a) = Var (g a) 
   fmap g (Not a) = Not (fmap g a)
   fmap g (a :||: b)  = fmap g a .||. fmap g b 
   fmap g (a :&&: b)  = fmap g a .&&. fmap g b
   fmap g (a :=>: b)  = fmap g a .=>. fmap g b
   fmap g (a :<=>: b) = fmap g a .<=>. fmap g b

instance Applicative Logic where 
   pure = Var 
   _ <*> T = T 
   T <*> _ = T 
   _ <*> F = F 
   F <*> _ = F 
   lg <*> Not a = Not (lg <*> a)
   Not lg <*> a = Not (lg <*> a)
   lg <*> (a :||: b)   = (lg <*> a) .||. (lg <*> b)
   (lg :||: lf) <*> a  = (lg <*> a) .||. (lf <*> a) 
   lg <*> (a :&&: b)   = (lg <*> a) .&&. (lg <*> b)
   (lg :&&: lf) <*> a  = (lg <*> a) .&&. (lf <*> a) 
   lg <*> (a :=>: b)   = (lg <*> a) .=>. (lg <*> b)
   (lg :=>: lf) <*> a  = (lg <*> a) .=>. (lf <*> a) 
   lg <*> (a :<=>: b)  = (lg <*> a) .<=>. (lg <*> b)
   (lg :<=>: lf) <*> a = (lg <*> a) .<=>. (lf <*> a) 

instance Monad Logic where
   T >>= f = T
   F >>= f = F
   Var a >>= f = f a 
   Not a >>= f = Not (a >>= f)
   (a :||: b) >>= f = (a >>= f) .||. (b >>=f)
   (a :&&: b) >>= f = (a >>= f) .&&. (b >>=f)
   (a :=>: b) >>= f = (a >>= f) .=>. (b >>=f)
   (a :<=>: b) >>= f = (a >>= f) .<=>. (b >>=f)

instance Foldable Logic where
   foldr f v  T = v
   foldr f v  F = v
   foldr f v  (Not a) = foldr f v a
   foldr f v (Var a)    = f a v
   foldr f v (t1 :||: t2) = foldr f (foldr f v t2) t1
   foldr f v (t1 :&&: t2) = foldr f (foldr f v t2) t1
   foldr f v (t1 :=>: t2) = foldr f (foldr f v t2) t1
   foldr f v (t1 :<=>: t2) = foldr f (foldr f v t2) t1

instance Traversable Logic where 
   traverse g (Var a)    = pure Var <*> g a 
   traverse g (Not a)    = pure Not <*> traverse g a
   traverse g (a :||: b) = 
      pure (.||.) <*> traverse g a <*> traverse g b 
   traverse g (a :&&: b) = 
      pure (.&&.) <*> traverse g a <*> traverse g b 
   traverse g (a :=>: b) = 
      pure (.=>.) <*> traverse g a <*> traverse g b 
   traverse g (a :<=>: b) = 
      pure (.<=>.) <*> traverse g a <*> traverse g b 

eval :: Logic Bool -> Bool
eval (a :||: b)  = eval a || eval b 
eval (a :&&: b)  = eval a && eval b 
eval (a :=>: b)  = (not (eval a)) || eval b 
eval (a :<=>: b) = ((eval a) && (eval b)) || (not (eval a) && not (eval b))
eval (Not a) = not (eval a) 
eval T = True
eval F = False
eval (Var a) = a 


bin :: Int -> [[Bool]]
bin n = (last . take (n + 1) . iterate bin') [[]]
   where 
      bin' ass = [a:as | as <- ass, a <- [True, False]] 


table :: Int -> Logic Int -> [([(Bool, Int)], Bool)]
table n l = [(zip bs as, nnd l bs) | bs <- bin n]
   where as = [1..n]
         nnd l bs = eval (l >>= f) 
            where 
            f a = if a <= n 
                  then Var (bs !! (a - 1))
                  else error "number of Var to large"

dnf :: Int -> Logic Int -> Logic Int
dnf n l = case [build t | (t, b) <- table n l, b]
            of [] -> F
               ls -> foldr1 (.||.) ls
   where 
      build t' = foldr1 (.&&.) (map toLog t')
      toLog (t1, t2) = if t1 
                       then (Var t2) 
                       else Not (Var t2)

cnf :: Int -> Logic Int -> Logic Int
cnf n l = case [build t | (t, b) <- table n l, not b]
            of [] -> T
               ls -> foldr1 (.&&.) ls
   where 
      build t' = foldr1 (.||.) (map toLog t')
      toLog (t1, t2) = if t1 
                       then (Var t2) 
                       else Not (Var t2)