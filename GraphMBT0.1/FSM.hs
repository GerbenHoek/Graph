module FSM where
import Trails
import Utils
import qualified Data.Map as Map
import Rules
import Data.Maybe
import InterStrat

data FSM s r = FSM { transitions :: Map.Map s [(Maybe r, s)]
                   , start :: s
                   , accept :: [s]
                   }
   deriving Show

instance Functor (FSM s) where
   fmap g (FSM t s a) = FSM (Map.map (imap g) t) s a 
      where imap g t' = [(g <$> mr, s) | (mr, s) <- t']

remainders :: Ord a => Trail a -> [Trail a]
remainders t = getFix f [t] 
   where 
      f ss = setNub $ 
         [s' | s <- ss, (r, s') <- firsts s] ++ ss

fsm :: (Show r, Ord r) => Trail r -> FSM Int r
fsm t = FSM transitions' start' accept' 
   where 
      transitions' = Map.fromList $ map trans rmd
      trans s = (n s, trs s)
      trs s = setNub $ 
                 [(Just r , n s') | (r, s') <- firsts s] ++
                    if empty s 
                    then [(Nothing, n s')| (r, s') <- firsts s] 
                    else []
      accept' = map n $ Succeed : [Many t' | Many t' <- rmd]
      start'  = n t
      n       = flip (index) rmd 
      rmd     = remainders t

pointTo :: Ord s => FSM s r -> FSM s r
pointTo (FSM t s a) = FSM t' s a
   where
      t'   = Map.fromList [(s, to s)| s <- Map.keys t]
      to s = [(mr, s')| (s', tr) <- Map.toList t, (mr, s) <- tr] 

bfsm :: FSM s (Rule a) -> FSM s (Rule a)
bfsm (FSM t s a) = FSM t' s a 
   where 
      t' = Map.map filter' t
      filter' xs = 
         [(mr, s) | (mr, s) <- xs, 
            fromMaybe False $ isBuggy <$> mr]

rfsm :: FSM s (Rule a) -> FSM s (Rule a)
rfsm (FSM t s a) = FSM t' s a 
   where 
      t' = Map.map filter' t
      filter' xs = 
         [(mr, s) | (mr, s) <- xs, 
            fromMaybe True $ (not . isBuggy) <$> mr]