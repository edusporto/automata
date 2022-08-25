module Automata.Conversion (module Automata.Conversion) where

import qualified Automata.DFA as D
import qualified Automata.NFA as N

dfaToNfa :: D.DFA s a -> N.NFA s a
dfaToNfa (D.DFA δ s e) = N.NFA newδ s e
  where
    newδ q a = case a of
      Nothing -> []
      Just sym -> [δ q sym]

nfaToDfa :: N.NFA s a -> D.DFA [s] a
nfaToDfa (N.NFA δ q e) = D.DFA δ' q' e'
  where
    δ' r' a = concat [δ r (Just a) | r <- r']
    q' = [q]
    e' = any e
