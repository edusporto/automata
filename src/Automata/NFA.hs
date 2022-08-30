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

data StateUnion s1 s2
  = Start
  | S1 s1
  | S2 s2

next :: (s -> Maybe a -> [s]) -> [s] -> a -> [s]
next δ states symbol =
  states >>= \state ->
    δ state (Just symbol) ++ next δ (δ state Nothing) symbol

run :: NFA s a -> [a] -> [s]
run nfa = foldl' (next (transition nfa)) [start nfa]

-- | Union of NFAs
--
-- Note that the resulting state set is a sum type of
-- a unit type, s1, and s2. This means that the resulting
-- amount of possible states is 1 + |s1| + |s2|.
--
-- This is different than the union operation for DFAs,
-- which results in product type of size |s1| * |s2|.
unionN :: NFA s1 a -> NFA s2 a -> NFA (StateUnion s1 s2) a
unionN (NFA δ1 s1 e1) (NFA δ2 s2 e2) = NFA δ Start end
  where
    δ q a = case q of
      Start -> case a of
        Nothing -> [S1 s1, S2 s2]
        _ -> []
      S1 q' -> map S1 (δ1 q' a)
      S2 q' -> map S2 (δ2 q' a)
    end q = case q of
      Start -> False
      S1 q' -> e1 q'
      S2 q' -> e2 q'
