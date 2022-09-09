{-# LANGUAGE MultiParamTypeClasses #-}

module Automata.Automaton (module Automata.Automaton) where

class Automaton n s a where
  accepts :: Foldable t => n s a -> t a -> Bool
