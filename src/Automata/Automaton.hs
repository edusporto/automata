module Automata.Automaton (module Automata.Automaton) where

class Automaton m where
  accepts :: m s a -> [a] -> Bool
