module Automata.Regular.Operations (module Automata.Regular.Operations) where

import Automata.Definitions.Set
import Automata.Regular.Conversion (dfaToNfa, nfaToDfa)
import Automata.Regular.DFA (DFA (DFA))
import Automata.Regular.NFA (NFA (NFA))
import Data.Maybe (isNothing)

-- | Union of DFAs
--
-- Note that the resulting state set is a pair of s1 and s2.
-- Since pairs are product types, the resulting amount of
-- possible states is |s1| * |s2|.
--
-- This is different than the union operation for NFAs,
-- which results in a sum type of size 1 + |s1| + |s2|.
unionD :: DFA s1 a -> DFA s2 a -> DFA (s1, s2) a
unionD (DFA δ1 q1 end1) (DFA δ2 q2 end2) = DFA δ (q1, q2) (Set end)
  where
    δ (r1, r2) a = (δ1 r1 a, δ2 r2 a)
    end (r1, r2) = end1 `contains` r1 || end2 `contains` r2

interD :: DFA s1 a -> DFA s2 a -> DFA (s1, s2) a
interD (DFA δ1 q1 end1) (DFA δ2 q2 end2) = DFA δ (q1, q2) (Set end)
  where
    δ (r1, r2) a = (δ1 r1 a, δ2 r2 a)
    end (r1, r2) = end1 `contains` r1 && end2 `contains` r2

-- TODO?: Don't use NFA for DFA concatenation
concatD :: DFA s1 a -> DFA s2 a -> DFA [Either s1 s2] a
concatD dfa1 dfa2 = nfaToDfa $ concatN (dfaToNfa dfa1) (dfaToNfa dfa2)

-- | Used as the return type for the union of NFAs.
--
-- Since it is a sum type, it has 1 + |s1| + |s2|
-- possible states.
data StateUnion s1 s2
  = Start
  | S1 s1
  | S2 s2

-- | Union of NFAs
--
-- Note that the resulting state set is a sum type of
-- a unit type, s1, and s2. This means that the resulting
-- amount of possible states is 1 + |s1| + |s2|.
--
-- This is different than the union operation for DFAs,
-- which results in product type of size |s1| * |s2|.
unionN :: NFA s1 a -> NFA s2 a -> NFA (StateUnion s1 s2) a
unionN (NFA δ1 s1 e1) (NFA δ2 s2 e2) = NFA δ Start (Set end)
  where
    δ q a = case q of
      Start -> case a of
        Nothing -> [S1 s1, S2 s2]
        _ -> []
      S1 q' -> map S1 (δ1 q' a)
      S2 q' -> map S2 (δ2 q' a)
    end q = case q of
      Start -> False
      S1 q' -> e1 `contains` q'
      S2 q' -> e2 `contains` q'

concatN :: NFA s1 a -> NFA s2 a -> NFA (Either s1 s2) a
concatN (NFA δ1 s1 e1) (NFA δ2 s2 e2) = NFA δ (Left s1) (Set end)
  where
    δ (Left q') a = ([Right s2 | isNothing a && e1 `contains` q']) ++ map Left (δ1 q' a)
    δ (Right q') a = map Right (δ2 q' a)
    end (Left _) = False
    end (Right q') = e2 `contains` q'
