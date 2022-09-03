{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Automata.Definitions.Set (module Automata.Definitions.Set) where

import Control.Applicative (Applicative (liftA2))
import qualified Data.Foldable
import Data.Universe (Universe (universe))

newtype Set s = Set {contains :: s -> Bool}
  deriving (Universe)

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

toList :: Universe s => Set s -> [s]
toList set = [s | s <- universe, set `contains` s]

foldlS :: Universe a => (b -> a -> b) -> b -> Set a -> b
foldlS f start set = Prelude.foldl f start (toList set)

foldrS :: Universe a => (a -> b -> b) -> b -> Set a -> b
foldrS f start set = Prelude.foldr f start (toList set)

foldl'S :: Universe a => (b -> a -> b) -> b -> Set a -> b
foldl'S f start set = Data.Foldable.foldl' f start (toList set)

instance Semigroup (Set s) where
  (<>) = union

instance Monoid (Set s) where
  mempty = Set (const False)

foldMapS :: (Universe s, Monoid m) => (s -> m) -> Set s -> m
foldMapS f = foldrS (mappend . f) mempty

anyS :: (Universe s) => (s -> Bool) -> Set s -> Bool
anyS f set = any (Set f `inter` set `contains`) universe

nonEmpty :: (Universe s) => Set s -> Bool
nonEmpty (Set set) = any set universe
