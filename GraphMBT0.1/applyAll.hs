module ApplyAll where 
import StrategyBas
import Utils 
import Rules
import InterStrat
import SimplinStrat
import qualified Data.Set as Set
import qualified Data.Map as Map

type Tracer a = (a, [[Rule a]])

liftToTracer :: Ord a => Rule a -> Rule (Tracer a)
liftToTracer r@(Rule b s f) = Rule b s f'
   where 
      f' (a, bs) = 
         [(a',  bs') | a' <- (f a) ] 
         where bs' = if b then map (r:) bs else bs

liftS :: Ord a => Strat a -> Strat (Tracer a)
liftS = fmap liftToTracer

diagnose :: Ord a => Strat a -> a -> a -> [[Rule a]]
diagnose s a1 a2 =  
   nubSub . concat . map snd . filter ((a2 ==) . fst) $ 
      applyAll (liftS s) (a1, [[]])
