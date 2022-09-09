{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Automata.Regular.NFA (module Automata.Regular.NFA) where

import Automata.Automaton (Automaton, accepts)
import Automata.Definitions.Set (Set, inter, nonEmpty)
import qualified Automata.Definitions.Set as S
import Data.Foldable (foldl')
import Data.Universe (Universe)

-- | Represents a Non-deterministic Finite Automaton.
--
-- The state set Q and alphabet set Σ are defined
-- by the type variables `s` and `a`.
data NFA s a = NFA
  { transition :: s -> Maybe a -> [s],
    start :: s,
    endings :: Set s
  }

instance Automaton NFA s a where
  accepts :: Foldable t => NFA s a -> t a -> Bool
  accepts nfa string =
    let lastStates = run nfa string
     in any (endings nfa `S.contains`) lastStates

next :: (s -> Maybe a -> [s]) -> [s] -> a -> [s]
next δ states symbol =
  states >>= \state ->
    δ state (Just symbol) ++ next δ (δ state Nothing) symbol

run :: Foldable t => NFA s a -> t a -> [s]
run nfa = foldl' (next (transition nfa)) [start nfa]

-- | Type algebra-driven representation of a non-deterministic
-- finite state automaton
--
-- The transition function in automata theory for an NFA
-- is of the type Q ⨯ (Σ ∪ ε) → P(Q), where P(Q) is the power set
-- of Q, that is, the set of all subsets of Q. The amount of
-- elements in the power set P(Q) is equal to 2^|Q|, where |Q|
-- is the amount of elements in Q.
--
-- The other definition for the NFA's transition function
-- returns a List, which is an infinite recursive type. The
-- power set could more accurately be represented by the
-- function s -> Bool, which is equivalent to `2^s` in type
-- algebra.
--
-- This is probably less efficient than the other version,
-- and needs to be tested.
data TypedNFA s a = TypedNFA
  { transitionT :: s -> Maybe a -> Set s,
    startT :: s,
    endingsT :: Set s
  }

instance (Universe s, Eq s) => Automaton TypedNFA s a where
  accepts :: Foldable t => TypedNFA s a -> t a -> Bool
  accepts nfa string =
    let lastStates = runT nfa string
     in nonEmpty (endingsT nfa `inter` lastStates)

nextT :: Universe s => (s -> Maybe a -> Set s) -> Set s -> a -> Set s
nextT δ states symbol =
  states `S.bind` \state ->
    δ state (Just symbol) `S.union` nextT δ (δ state Nothing) symbol

runT :: (Universe s, Eq s) => Foldable t => TypedNFA s a -> t a -> Set s
runT nfa = foldl' (nextT (transitionT nfa)) (S.singleton (startT nfa))
