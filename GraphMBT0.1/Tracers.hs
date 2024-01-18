module Tracers where
import Data.Maybe
import qualified Data.Map as Map
import Rules
import Utils --hiding (nubSub)
import FSM 
import StrategyBas
import SimplinStrat
import Refinement
import InterRules hiding (start)
import InterStrat
--import NubSub (nubSub)


type Tracers a = Map.Map a ([(Rule a, Int)], [[Rule a]])

type Tracer a = (a, ([(Rule a, Int)],[[Rule a]]))

type StatedTracers s a = Map.Map s (Tracers a)

embed :: s -> a -> StatedTracers s a
embed s a = 
   Map.singleton s $ Map.singleton a ([], [[]])

flatten :: StatedTracers s a -> [(s, [Tracer a])]
flatten = Map.toList . Map.map Map.toList


insertT :: Ord a => Tracer a -> Tracers a -> Tracers a
insertT (a, rp@(rs, bs)) trs = case Map.lookup a trs of 
   Nothing       
      -> Map.insert a rp trs 
   Just (rs', bs') 
      -> Map.insert a (rs', nubSub $ bs ++ bs') trs
           
placeTracer 
   :: (Ord a , Ord s) => (s, Tracer a) -> 
      StatedTracers s a -> StatedTracers s a
placeTracer (s, t@(a, rp)) trs =
   case Map.lookup s trs of 
      Nothing -> Map.insert s (Map.singleton a rp) trs 
      Just ts -> Map.insert s (insertT t ts) trs

fromListT :: (Ord a , Ord s) => 
   [(s, Tracer a)] -> StatedTracers s a
fromListT = foldr placeTracer Map.empty

liftToTracer :: Ord a => Rule a -> Rule (Tracer a)
liftToTracer r@(Rule b s f) = Rule b s f'
   where 
      f' (a, (rs, bs)) = 
         [(a', ((r, n):rs, bs')) | 
            (a',n) <- zip (f a) [0..]] 
         where bs' = if b then map (r:) bs else bs

advance
  :: (Ord a, Ord s, Show s) =>
     FSM s (Rule a) -> StatedTracers s a -> StatedTracers s a
advance fsm = fromListT . advance' fsm
   where
      advance' fsm st = do
            (s, tr)  <- flatten st
            (r, s')  <- fromMaybe (err s) $ Map.lookup s (transitions fsm)
            t        <- tr       
            t'       <- fromMaybe [t] $ flip applyRule t . liftToTracer <$> r
            return (s', t')
         where 
            err s = error $ "state " ++ show s ++ " not in fsm" 

collect :: 
   (Ord a, Ord s, Show s) => 
      FSM s (Rule a) -> 
         ([Tracers a], StatedTracers s a) ->
            ([Tracers a], StatedTracers s a) 
collect fsm (tr, st) = (tr', st')
   where 
      tr' = (accepted st) ++ tr 
      st' = advance fsm st
      accepted = 
         Map.elems . Map.filterWithKey 
            (\s _ -> s `elem` (accept fsm))

applyAllM
  :: (Ord a, Ord s, Show s) =>
     FSM s (Rule a) -> a -> Tracers a
applyAllM fsm a = 
   Map.unionsWith nubSub' $
   fst . iterWhile (not.null.snd) (collect fsm) $ 
   ([],embed (start fsm) a)
   where
      nubSub' (rs, bs) (rs', bs') = (rs, nubSub $ bs ++ bs') 


diagnose fsm a1 a2 = 
   Map.lookup a2 (applyAllM fsm a1) 
