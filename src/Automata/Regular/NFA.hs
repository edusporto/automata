{-# LANGUAGE InstanceSigs #-}

module Automata.Regular.NFA (module Automata.Regular.NFA) where

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

-- | Experimental definition for an NFA.
--
-- The transition function in automata theory for an NFA
-- is of the type Q ⨯ Σ → P(Q), where P(Q) is the power set
-- of Q, that is, the set of all subsets of Q. The amount of
-- elements in the power set P(Q) is equal to 2^|Q|, where |Q|
-- is the amount of elements in Q.
--
-- The current definition for the NFA's transition function
-- returns a List, which is an infinite recursive type. The
-- power set could more accurately be represented by the
-- function (s -> Bool), which is equivalent to `2^s` in type
-- algebra.
--
-- Although promising, I haven't yet been able to come up with
-- a definition for `accepts` for this definition of δ.
data TypedNFA s a = TypedNFA
  { transitionT :: s -> Maybe a -> (s -> Bool),
    startT :: s,
    endingsT :: s -> Bool
  }

instance Automaton NFA where
  accepts :: Foldable t => NFA s a -> t a -> Bool
  accepts nfa string =
    let lastStates = run nfa string
     in any (endings nfa) lastStates

next :: (s -> Maybe a -> [s]) -> [s] -> a -> [s]
next δ states symbol =
  states >>= \state ->
    δ state (Just symbol) ++ next δ (δ state Nothing) symbol

run :: Foldable t => NFA s a -> t a -> [s]
run nfa = foldl' (next (transition nfa)) [start nfa]
