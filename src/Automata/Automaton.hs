module Automata.Automaton (module Automata.Automaton) where

class Automaton n where
  accepts :: Foldable t => n s a -> t a -> Bool
