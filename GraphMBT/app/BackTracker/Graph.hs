module BackTracker.Graph where
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

newtype Graph n e = G {getG :: Map n (Set (e, n))}
   deriving Show

empty :: Graph n e
empty = G Map.empty 

insert :: (Ord n, Ord e) -- (backward insert)
   => (n, e, n) -> Graph n e -> Graph n e
insert (n, e, m) = 
  G . Map.insertWith (Set.union) m (Set.singleton (e, n)) . getG

build :: (Ord n, Ord e) 
   => [(n,e,n)] -> Graph n e 
build = foldr insert empty 

augment :: (Ord n, Ord e) 
   => [(n,e,n)] -> Graph n e -> Graph n e 
augment egs g = foldr insert g egs 

backwards :: (Ord n, Ord e) 
   => n -> Graph n e -> Maybe (Set (e, n))
backwards n = Map.lookup n . getG