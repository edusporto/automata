module Automata.Regular.Conversion (module Automata.Regular.Conversion) where

import Automata.Definitions.Set
import qualified Automata.Regular.DFA as D
import qualified Automata.Regular.NFA as N
import Data.Universe (Universe)

dfaToNfa :: D.DFA s a -> N.NFA s a
dfaToNfa (D.DFA δ s e) = N.NFA δ' s e
  where
    δ' q a = case a of
      Nothing -> []
      Just sym -> [δ q sym]

nfaToDfa :: N.NFA s a -> D.DFA [s] a
nfaToDfa (N.NFA δ q e) = D.DFA δ' q' e'
  where
    δ' r' a = N.next δ r' a
    q' = [q]
    e' = Set (any (contains e))

dfaToNfaT :: Eq s => D.DFA s a -> N.TypedNFA s a
dfaToNfaT (D.DFA δ s e) = N.TypedNFA δ' s e
  where
    δ' q a = case a of
      Nothing -> Set (const False)
      Just sym -> singleton (δ q sym)

nfaTToDfa :: (Universe s, Eq s) => N.TypedNFA s a -> D.DFA (Set s) a
nfaTToDfa (N.TypedNFA δ q e) = D.DFA δ' q' e'
  where
    δ' r' a = N.nextT δ r' a
    q' = singleton q
    -- e' = Set $ \set -> any (contains e) (toList set)
    -- e' = Set $ \set -> nonEmpty (e `inter` set)
    e' = Set (anyS (contains e))
