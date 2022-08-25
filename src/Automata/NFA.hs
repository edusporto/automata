{-# LANGUAGE InstanceSigs #-}

module Automata.NFA (module Automata.NFA) where

import Automata.Automaton (Automaton, accepts)
import Data.Foldable (foldl')

-- | Represents a Non-deterministic Finite Automaton.
--
-- The state set Q and alphabet set Σ are defined
-- by the type variables `s` and `a`.
data NFA s a = NFA
  { transition :: s -> Maybe a -> [s],
    start :: s,
    endings :: s -> Bool
  }

instance Automaton NFA where
  accepts :: NFA s a -> [a] -> Bool
  accepts nfa string =
    let lastStates = run nfa string
     in any (endings nfa) lastStates

next :: (s -> Maybe a -> [s]) -> [s] -> a -> [s]
next δ states symbol =
  states >>= \state ->
    δ state (Just symbol) ++ next δ (δ state Nothing) symbol

run :: NFA s a -> [a] -> [s]
run nfa = foldl' (next (transition nfa)) [start nfa]
