module FSMMaker.Trails where
import Data.Maybe (fromMaybe)
import Control.Monad
import Data.Foldable 
import Utils (setNub)

data Trail a = Trail a :*: Trail a
           | Trail a :|: Trail a
           | Many (Trail a)
           | R a
           | Lbl String (Trail a)
           | Succeed
           | Fail
 deriving (Eq, Ord, Read, Show)
  
-- constructs a sequence like :*:, but maintains invariants for datatype
(.*.) :: Trail a -> Trail a -> Trail a
Succeed .*. b  = b
a .*. Succeed  = a
(a :*: b) .*. c = a .*. (b .*. c)
a .*. b  = a :*: b

(<|>) :: Trail a -> Trail a -> Trail a
(a :|: b) <|> c = a <|> (b <|> c)
a <|> b = a :|: b

-- parsing a trail 
empty :: Trail a -> Bool
empty Succeed    = True
empty Fail       = False
empty (R r)      = False
empty (s:|:t)    = empty s || empty t
empty (s:*:t)    = empty s && empty t
empty (Many s)   = True
empty (Lbl st t) = empty t 

firsts :: Trail a -> [(a, Trail a)]
firsts Succeed    = []
firsts Fail       = []
firsts (R r)      = [(r, Succeed)]
firsts (s:|:t)    = firsts s ++ firsts t
firsts (s:*:t)    = [(r, s'.*.t) | (r, s') <- firsts s] ++ 
                       if empty s then firsts t else []             
firsts (Many s)   = 
   [(r, s' .*. Many s) | (r, s') <- firsts s] 
firsts (Lbl st t) = firsts t 

instance Functor Trail where
   fmap g Succeed   = Succeed
   fmap g Fail      = Fail
   fmap g (R a)     = R (g a)
   fmap g (a:|:b)   = fmap g a <|> fmap g b 
   fmap g (a:*:b)   = fmap g a .*. fmap g b
   fmap g (Many a)  = Many (fmap g a) 
   fmap g (Lbl s a) = Lbl s (fmap g a) 

instance  Applicative Trail where
   pure = R 
   (R g) <*> x    = fmap g x 
   Succeed <*> _  = Succeed
   _ <*> Succeed  = Succeed
   _ <*> Fail     = Fail
   Fail <*> _     = Fail
   pg <*> Many a  = Many (pg <*> a) 
   pg <*> Lbl s a = Lbl s (pg <*> a)  
   pg <*> (x :|: y)  = (pg <*> x) <|> (pg <*> y)
   pg <*> (x :*: y)  = (pg <*> x) .*. (pg <*> y) 
   (pg :|: pf) <*> x = (pg <*> x) <|> (pf <*> x) 
   (pg :*: pf) <*> x = (pg <*> x) .*. (pf <*> x)  
   
instance Monad Trail where
   Succeed >>= f  = Succeed
   Fail >>= f     = Fail 
   (R a) >>= f    = f a
   Many t >>= f   = Many (t >>= f)  
   Lbl s t >>= f  = Lbl s (t >>= f)    
   t1 :|: t2 >>= f = (t1 >>= f) <|> (t2 >>= f)
   t1 :*: t2 >>= f = (t1 >>= f) .*. (t2 >>= f)

instance Foldable Trail where
   foldr f v  Succeed    = v
   foldr f v  Fail       = v
   foldr f v (R a)       = f a v
   foldr f v (t1 :*: t2) = foldr f (foldr f v t2) t1
   foldr f v (t1 :|: t2) = foldr f (foldr f v t2) t1
   foldr f v (Many t)    = foldr f v t
   foldr f v (Lbl s t)   = foldr f v t

filterT :: (a -> Bool) -> Trail a -> Trail a
filterT b t = t >>= f
   where f a = if b a 
               then R a 
               else Succeed

form :: Trail a -> Trail b -> Bool
form t1 t2 = fmap (const 0) t1 == fmap (const 0) t2

toListS :: Ord a => Trail a -> [a]
toListS = setNub . toList
        