module Automata.Automaton (module Automata.Automaton) where

class Automaton n where
  accepts :: n s a -> [a] -> Bool
