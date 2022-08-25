{-# LANGUAGE MultiParamTypeClasses #-}

module Automata.Automaton (module Automata.Automaton) where

class Automaton m s a where
  accepts :: m s a -> [a] -> Bool
