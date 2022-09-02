{-# LANGUAGE InstanceSigs #-}

module Automata.Regular.DFA (module Automata.Regular.DFA) where

import Automata.Automaton (Automaton, FiniteAutomaton, accepts, acceptsT)
import Automata.Definitions.Set
import Data.Foldable (foldl')

-- | Represents a Deterministic Finite Automaton.
--
-- The state set Q and alphabet set Σ are defined
-- by the type variables `s` and `a`.
data DFA s a = DFA
  { transition :: s -> a -> s,
    start :: s,
    endings :: Set s
  }

instance Automaton DFA where
  accepts :: Foldable t => DFA s a -> t a -> Bool
  accepts dfa string =
    let lastState = run dfa string
     in endings dfa `contains` lastState

instance FiniteAutomaton DFA where
  acceptsT = accepts

run :: Foldable t => DFA s a -> t a -> s
run dfa = foldl' (transition dfa) (start dfa)
