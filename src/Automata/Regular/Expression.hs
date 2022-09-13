{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Automata.Regular.Expression (module Automata.Regular.Expression) where

import Automata.Definitions.Set (Set (Set, contains))
import Automata.Regular.NFA (NFA (NFA))
import Automata.Regular.Operations (StateUnion, concatN, starN, unionN)

data Regex a
  = Symbol a
  | Empty
  | Null
  | Union (Regex a) (Regex a)
  | Concat (Regex a) (Regex a)
  | Star (Regex a)
  deriving (Show, Eq)

type TwoStates = Bool

type OneState = ()

data RegexState
  = SymbolS TwoStates
  | EmptyS OneState
  | NullS OneState
  | UnionS (StateUnion RegexState RegexState)
  | ConcatS (Either RegexState RegexState)
  | StarS (Maybe RegexState)
  deriving (Show, Eq)

-- TODO: Refactor `mapTypeS`
evalN :: Eq a => Regex a -> NFA RegexState a
evalN (Symbol sy) = NFA δ s1 end
  where
    δ (SymbolS False) (Just a) = [SymbolS True | a == sy]
    δ _ _ = []
    s1 = SymbolS False
    end = Set (\case SymbolS s -> s; _ -> False)
evalN Empty = NFA δ s1 end
  where
    δ _ _ = []
    s1 = EmptyS ()
    end = Set (const True)
evalN Null = NFA δ s1 end
  where
    δ _ _ = []
    s1 = NullS ()
    end = Set (const False)
evalN (Union r1 r2) = mapUnionS (evalN r1 `unionN` evalN r2)
  where
    mapUnionS (NFA δ' s1' end') = NFA δ s1 end
      where
        δ (UnionS s) a = map UnionS (δ' s a)
        δ _ _ = []
        s1 = UnionS s1'
        end = Set (\case UnionS s -> end' `contains` s; _ -> False)
evalN (Concat r1 r2) = mapConcatS (evalN r1 `concatN` evalN r2)
  where
    mapConcatS (NFA δ' s1' end') = NFA δ s1 end
      where
        δ (ConcatS s) a = map ConcatS (δ' s a)
        δ _ _ = []
        s1 = ConcatS s1'
        end = Set (\case ConcatS s -> end' `contains` s; _ -> False)
evalN (Star r) = mapStarS (starN (evalN r))
  where
    mapStarS (NFA δ' s1' end') = NFA δ s1 end
      where
        δ (StarS s) a = map StarS (δ' s a)
        δ _ _ = []
        s1 = StarS s1'
        end = Set (\case StarS s -> end' `contains` s; _ -> False)
