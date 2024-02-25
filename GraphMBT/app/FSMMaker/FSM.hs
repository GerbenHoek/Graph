module FSMMaker.FSM where
import FSMMaker.Trails
import FSMMaker.Rules
import Utils
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (delete)
import SimpExpr.SimpLinStrat

import Data.Maybe

data FSM s r = FSM { transitions :: Map s [(Maybe r, s)]
                   , start :: s
                   , accept :: [s]
                   }
   deriving Show

type Trans s r = Map s [(Maybe r, s)]
type TransL s r = [(s, Maybe r, s)]

instance Functor (FSM s) where
   fmap g (FSM t s a) = FSM (Map.map (imap g) t) s a 
      where imap g t' = [(g <$> mr, s) | (mr, s) <- t']

flatten :: Trans s r -> TransL s r 
flatten tr = [(s, mr, t) | (s, rs) <- Map.toList tr, (mr, t) <- rs]

insert :: (Ord s, Ord r) => (s, Maybe r, s) -> Trans s r -> Trans s r
insert (s, mr, t) tr = Map.insertWith ins s [(mr, t)] tr 
   where ins as bs = setNub (as ++ bs) 

endPoints :: (Ord s, Ord r) => TransL s r -> Trans s r
endPoints trL = Map.fromList $
   [(t,[])| (s, mr, t) <- trL, not $ t `elem` [s| (s, mr, t) <- trL]] 

build :: (Ord s, Ord r) => TransL s r -> Trans s r
build trL = foldr insert (endPoints trL) trL

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
      accept' = map n $ [Many t' | Many t' <- rmd] ++ [Succeed| Succeed <- rmd]
      start'  = n t
      n       = flip (index) rmd 
      rmd     = remainders t


relable :: Eq s => s -> s -> TransL s r -> TransL s r 
relable s t tr = [(rel a, mr, rel b) | (a, mr, b) <- tr]
   where 
      rel a  = if a == t then s else a

labeler :: (Ord s, Ord r) 
   => (TransL s r, s, [s]) -> (TransL s r, s, [s]) 
labeler (trL, st, ac) =  
   case [(s, t) | (s, Nothing, t) <-  trL] 
   of 
   []        -> (trL, st, ac)
   (s,t):sts -> labeler (trL', st', ac')
      where 
         trL' = relable s t (delete (s, Nothing, t) trL)
         st' = if t == st then s else st
         ac' = if t `elem` ac then s:(delete t ac) else ac 

merge :: (Ord s, Ord r) => FSM s r -> FSM s r
merge (FSM tr st ac) = FSM (build tr') st' ac' 
   where 
      (tr', st', ac') = labeler (flatten tr, st, ac)

--sink :: (Num s, Ord s, Ord r) => FSM s r -> FSM s r
--sink (FSM tr st ac) = (FSM tr' st [fresh]) 
--   where 
--      tr' = Map.insert fresh [] $ 
--         augment [(s, Nothing, fresh) | s <- ac] tr 
--      fresh = (maximum $ Map.keys tr) + 1 

reverseFSM :: Ord s => FSM s r -> FSM s r
reverseFSM (FSM t s a) = FSM t' s a
   where
      t'   = Map.fromList [(s, to s)| s <- Map.keys t]
      to s = [(mr, s')| (s', tr) <- Map.toList t, (mr, s) <- tr] 

bfsm :: FSM s (Rule a) -> FSM s (Rule a)
bfsm (FSM t s a) = FSM t' s a 
   where 
      t' = Map.map filter' t
      filter' xs = 
         [(mr, s) | (mr, s) <- xs, 
            maybe False isBuggy mr]

rfsm :: FSM s (Rule a) -> FSM s (Rule a)
rfsm (FSM t s a) = FSM t' s a 
   where 
      t' = Map.map filter' t
      filter' xs = 
         [(mr, s) | (mr, s) <- xs, 
            maybe True (not . isBuggy) mr]