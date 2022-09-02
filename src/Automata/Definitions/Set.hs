module Automata.Definitions.Set (module Automata.Definitions.Set) where

import Control.Applicative (Applicative (liftA2))
import Data.Universe (Universe (universe))

newtype Set s = Set {contains :: s -> Bool}

union :: Set s -> Set s -> Set s
union (Set f) (Set g) = Set (liftA2 (||) f g)

inter :: Set s -> Set s -> Set s
inter (Set f) (Set g) = Set (liftA2 (&&) f g)

isIn :: s -> Set s -> Bool
isIn = flip contains

singleton :: Eq s => s -> Set s
singleton s = Set (== s)

-- It is impossible to write a Functor for this definition of a set!
-- instance Functor Set where
--   fmap f (Set set) = Set (set . f)

-- | Bind operation for non-deterministic computations over a Set
--
-- Thanks to Daniel Wagner for coming up with this function at
-- https://stackoverflow.com/questions/73576728/non-determinism-on-a-set-defined-by-the-characteristic-function
bind :: Universe a => Set a -> (a -> Set b) -> Set b
bind (Set m) f = Set (\b -> any ($b) [bs | a <- universe, m a, let Set bs = f a])
