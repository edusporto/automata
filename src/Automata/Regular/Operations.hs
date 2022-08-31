module Automata.Regular.Operations (module Automata.Regular.Operations) where

import Automata.Regular.DFA (DFA (DFA))
import qualified Automata.Regular.DFA as D
import Automata.Regular.NFA (NFA (NFA))
import qualified Automata.Regular.NFA as N

-- | Union of DFAs
--
-- Note that the resulting state set is a pair of s1 and s2.
-- Since pairs are product types, the resulting amount of
-- possible states is |s1| * |s2|.
--
-- This is different than the union operation for NFAs,
-- which results in a sum type of size 1 + |s1| + |s2|.
unionD :: DFA s1 a -> DFA s2 a -> DFA (s1, s2) a
unionD (DFA δ1 q1 end1) (DFA δ2 q2 end2) = DFA δ (q1, q2) end
  where
    δ (r1, r2) a = (δ1 r1 a, δ2 r2 a)
    end (r1, r2) = end1 r1 || end2 r2

interD :: DFA s1 a -> DFA s2 a -> DFA (s1, s2) a
interD (DFA δ1 q1 end1) (DFA δ2 q2 end2) = DFA δ (q1, q2) end
  where
    δ (r1, r2) a = (δ1 r1 a, δ2 r2 a)
    end (r1, r2) = end1 r1 && end2 r2

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
unionN (NFA δ1 s1 e1) (NFA δ2 s2 e2) = NFA δ Start end
  where
    δ q a = case q of
      Start -> case a of
        Nothing -> [S1 s1, S2 s2]
        _ -> []
      S1 q' -> map S1 (δ1 q' a)
      S2 q' -> map S2 (δ2 q' a)
    end q = case q of
      Start -> False
      S1 q' -> e1 q'
      S2 q' -> e2 q'
