{-# LANGUAGE InstanceSigs #-}

module Automata.DFA (module Automata.DFA) where

import Automata.Automaton (Automaton, accepts)
import Data.Foldable (foldl')

-- | Represents a Deterministic Finite Automaton.
--
-- The state set Q and alphabet set Σ are defined
-- by the type variables `s` and `a`.
data DFA s a = DFA
  { transition :: s -> a -> s,
    start :: s,
    endings :: s -> Bool
  }

instance Automaton DFA where
  accepts :: Foldable t => DFA s a -> t a -> Bool
  accepts dfa string =
    let lastState = run dfa string
     in endings dfa lastState

run :: Foldable t => DFA s a -> t a -> s
run dfa = foldl' (transition dfa) (start dfa)

-- | Union of DFAs
--
-- Note that the resulting state set is a pair of s1 and s2.
-- Since pairs are product types, the resulting amount of
-- possible states is |s1| * |s2|.
--
-- This is different than the union operation for NFAs,
-- which results in a sum type of size 1 + |s1| + |s2|.
unionD :: DFA s1 a -> DFA s2 a -> DFA (s1, s2) a
unionD (DFA δ1 q1 end1) (DFA δ2 q2 end2) = DFA δ (q1, q2) end
  where
    δ (r1, r2) a = (δ1 r1 a, δ2 r2 a)
    end (r1, r2) = end1 r1 || end2 r2

interD :: DFA s1 a -> DFA s2 a -> DFA (s1, s2) a
interD (DFA δ1 q1 end1) (DFA δ2 q2 end2) = DFA δ (q1, q2) end
  where
    δ (r1, r2) a = (δ1 r1 a, δ2 r2 a)
    end (r1, r2) = end1 r1 && end2 r2
