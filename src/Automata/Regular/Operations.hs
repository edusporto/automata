{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Automata.Regular.Operations (module Automata.Regular.Operations) where

import Automata.Definitions.Set
import Automata.Regular.Conversion (dfaToNfa, nfaToDfa)
import Automata.Regular.DFA (DFA (DFA))
import Automata.Regular.NFA (NFA (NFA), TypedNFA (TypedNFA))
import Data.Maybe (isJust, isNothing)
import Data.Universe (Universe)

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

-- TODO?: Don't use NFA for DFA star
starD :: DFA s a -> DFA [Maybe s] a
starD = nfaToDfa . starN . dfaToNfa

-- | Used as the return type for the union of NFAs.
--
-- Since it is a sum type, it has 1 + |s1| + |s2|
-- possible states.
data StateUnion s1 s2
  = Start
  | S1 s1
  | S2 s2
  deriving (Show, Eq, Universe)

instance forall s1 s2. (Bounded s2) => Bounded (StateUnion s1 s2) where
  minBound = Start
  maxBound = S2 (maxBound @s2)

instance forall s1 s2. (Bounded s1, Bounded s2, Enum s1, Enum s2) => Enum (StateUnion s1 s2) where
  toEnum i
    | i == 0 = Start
    | i <= endS1 = S1 (toEnum @s1 (i - startS1))
    | i <= endS2 = S2 (toEnum @s2 (i - startS2))
    | otherwise = error "invalid bound for enum"
    where
      startS1 = 1
      endS1 = startS1 + fromEnum (maxBound @s1)
      startS2 = endS1 + 1
      endS2 = startS2 + fromEnum (maxBound @s2)
  fromEnum su = case su of
    Start -> 0
    S1 s1 -> startS1 + fromEnum s1
    S2 s2 -> startS2 + fromEnum s2
    where
      startS1 = 1
      endS1 = startS1 + fromEnum (maxBound @s1)
      startS2 = endS1 + 1

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

starN :: NFA s a -> NFA (Maybe s) a
starN (NFA δ1 s1 e1) = NFA δ Nothing (Set end)
  where
    δ Nothing (Just _) = []
    δ Nothing Nothing = [Just s1]
    δ (Just q) a
      | e1 `contains` q && isJust a = Just s1 : map Just (δ1 q a)
      | otherwise = map Just (δ1 q a)
    end s = maybe True (e1 `contains`) s

-- | Union of TypedNFAs
unionNT ::
  (Universe s1, Universe s2, Eq s1, Eq s2) =>
  TypedNFA s1 a ->
  TypedNFA s2 a ->
  TypedNFA (StateUnion s1 s2) a
unionNT (TypedNFA δ1 s1 e1) (TypedNFA δ2 s2 e2) = TypedNFA δ Start (Set end)
  where
    δ q a = case q of
      Start -> case a of
        Nothing -> singleton (S1 s1) <> singleton (S2 s2)
        _ -> emptySet
      S1 q' -> mapS S1 (δ1 q' a)
      S2 q' -> mapS S2 (δ2 q' a)
    end q = case q of
      Start -> False
      S1 q' -> e1 `contains` q'
      S2 q' -> e2 `contains` q'

concatNT ::
  (Universe s1, Universe s2, Eq s1, Eq s2) =>
  TypedNFA s1 a ->
  TypedNFA s2 a ->
  TypedNFA (Either s1 s2) a
concatNT (TypedNFA δ1 s1 e1) (TypedNFA δ2 s2 e2) = TypedNFA δ (Left s1) (Set end)
  where
    δ (Left q') a = (fromList [Right s2 | isNothing a && e1 `contains` q']) <> mapS Left (δ1 q' a)
    δ (Right q') a = mapS Right (δ2 q' a)
    end (Left _) = False
    end (Right q') = e2 `contains` q'

starNT :: (Universe s, Eq s) => TypedNFA s a -> TypedNFA (Maybe s) a
starNT (TypedNFA δ1 s1 e1) = TypedNFA δ Nothing (Set end)
  where
    δ Nothing (Just _) = emptySet
    δ Nothing Nothing = singleton (Just s1)
    δ (Just q) a
      | e1 `contains` q && isJust a = singleton (Just s1) `union` mapS Just (δ1 q a)
      | otherwise = mapS Just (δ1 q a)
    end s = maybe True (e1 `contains`) s
