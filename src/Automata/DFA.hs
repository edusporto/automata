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
  accepts :: DFA s a -> [a] -> Bool
  accepts dfa string =
    let lastState = run dfa string
     in endings dfa lastState

run :: DFA s a -> [a] -> s
run dfa = foldl' (transition dfa) (start dfa)

inter :: DFA s1 a -> DFA s2 a -> DFA (s1, s2) a
inter (DFA δ1 q1 end1) (DFA δ2 q2 end2) = DFA δ (q1, q2) end
  where
    δ (r1, r2) a = (δ1 r1 a, δ2 r2 a)
    end (r1, r2) = end1 r1 && end2 r2

(∩) :: DFA s1 a -> DFA s2 a -> DFA (s1, s2) a
(∩) = inter

union :: DFA s1 a -> DFA s2 a -> DFA (s1, s2) a
union (DFA δ1 q1 end1) (DFA δ2 q2 end2) = DFA δ (q1, q2) end
  where
    δ (r1, r2) a = (δ1 r1 a, δ2 r2 a)
    end (r1, r2) = end1 r1 || end2 r2

(∪) :: DFA s1 a -> DFA s2 a -> DFA (s1, s2) a
(∪) = inter
