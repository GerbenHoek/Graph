module ApplyAll where 
import Data.Maybe
import FSMMaker.StrategyBas
import FSMMaker.Rules
import Utils 

type Tracer a = (a, [[Rule a]])

liftToTracer :: Ord a => Rule a -> Rule (Tracer a)
liftToTracer r@(Rule b s f) = Rule b s f'
   where 
      f' (a, bs) = 
         [(a',  bs') | a' <- (f a) ] 
         where bs' = if b then map (r:) bs else bs

liftS :: Ord a => Strat a -> Strat (Tracer a)
liftS = fmap liftToTracer

diagnose :: Ord a => Strat a -> a -> a -> Maybe [[Rule a]]
diagnose s a1 a2 =  
   case nubSub . concat . map snd . filter ((a2 ==) . fst) $ 
      applyAll (liftS s) (a1, [[]])
   of []  -> Nothing
      rls -> Just rls 