module Automata.Automaton (module Automata.Automaton) where

import Data.Universe (Universe)

class FiniteAutomaton n where
  acceptsT :: (Universe s, Eq s, Foldable t) => n s a -> t a -> Bool

class Automaton n where
  accepts :: Foldable t => n s a -> t a -> Bool
