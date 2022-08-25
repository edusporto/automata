module Automata.NFA (module Automata.NFA) where

import Data.Foldable (foldl')

import qualified Automata.DFA as DFA

-- | Represents a Non-deterministic Finite Automaton.
--
-- The state set Q and alphabet set Σ are defined
-- by the type variables `s` and `a`.
data NFA s a = NFA
  { transition :: s -> Maybe a -> [s],
    start :: s,
    endings :: s -> Bool
  }

next :: (s -> Maybe a -> [s]) -> [s] -> a -> [s]
next δ states symbol =
  states >>= \state ->
    δ state (Just symbol) ++ next δ (δ state Nothing) symbol

run :: NFA s a -> [a] -> [s]
run nfa = foldl' (next (transition nfa)) [start nfa]

accepts :: NFA s a -> [a] -> Bool
accepts nfa string =
  let lastStates = run nfa string
   in any (endings nfa) lastStates

nfaIntoDfa :: NFA s a -> DFA.DFA s a
nfaIntoDfa = error "TODO"
