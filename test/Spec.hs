{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}

import Automata.Automaton (Automaton (accepts), FiniteAutomaton (acceptsT))
import Automata.Definitions.Set (Set (Set))
import Automata.Regular.Conversion
import Automata.Regular.DFA
import Automata.Regular.NFA
import Data.Map (Map, fromList, (!))
import Data.Universe (Universe)
import Test.HUnit

data Binary = Zero | One
  deriving (Eq)

instance Show Binary where
  show Zero = "0"
  show One = "1"

binarize :: [Char] -> [Binary]
binarize =
  map
    ( \case
        '0' -> Zero
        '1' -> One
        _ -> error "invalid binary digit"
    )

example1 :: DFA String Char
example1 =
  DFA
    (\q a -> transitions ! q ! a)
    "q"
    (Set (== "q001"))
  where
    transitions :: Map String (Map Char String)
    transitions =
      fromList
        [ ("q", fromList [('0', "q0"), ('1', "q")]),
          ("q0", fromList [('0', "q00"), ('1', "q")]),
          ("q00", fromList [('0', "q00"), ('1', "q001")]),
          ("q001", fromList [('0', "q001"), ('1', "q001")])
        ]

try :: (Automaton n, Show a) => String -> Bool -> n s a -> [a] -> Test
try name bool automaton str =
  TestCase $ assertEqual (name ++ " " ++ show str) bool (accepts automaton str)

tryT :: (Universe s, Eq s, FiniteAutomaton n, Show a) => String -> Bool -> n s a -> [a] -> Test
tryT name bool automaton str =
  TestCase $ assertEqual (name ++ " " ++ show str) bool (acceptsT automaton str)

example1Tests :: Test
example1Tests =
  TestList
    [ try "dfa example1" True example1 "001",
      try "dfa example1" True example1 "0010",
      try "dfa example1" True example1 "0101010100100",
      try "dfa example1" False example1 "10000000000",
      try "nfa example1" True (dfaToNfa example1) "001",
      try "nfa example1" True (dfaToNfa example1) "0010",
      try "nfa example1" True (dfaToNfa example1) "0101010100100",
      try "nfa example1" False (dfaToNfa example1) "10000000000"
    ]

data Example2States = Q1 | Q2 | Q3 | Q4
  deriving (Eq, Show, Enum, Bounded, Universe)

example2 :: NFA Example2States Binary
example2 =
  NFA
    f
    Q1
    (Set (== Q4))
  where
    f = \case
      Q1 -> \case
        Just Zero -> [Q1]
        Just One -> [Q1, Q2]
        Nothing -> []
      Q2 -> \case
        Just Zero -> [Q3]
        Just One -> []
        Nothing -> [Q3]
      Q3 -> \case
        Just Zero -> []
        Just One -> [Q4]
        Nothing -> []
      Q4 -> \case
        Just Zero -> [Q4]
        Just One -> [Q4]
        Nothing -> []

example2T :: TypedNFA Example2States Binary
example2T =
  TypedNFA
    f
    Q1
    (Set (== Q4))
  where
    f = \case
      Q1 -> \case
        Just Zero -> Set $ \case
          Q1 -> True
          _ -> False
        Just One -> Set $ \case
          Q1 -> True
          Q2 -> True
          _ -> False
        Nothing -> Set $ const False
      Q2 -> \case
        Just Zero -> Set $ \case
          Q3 -> True
          _ -> False
        Just One -> Set $ const False
        Nothing -> Set $ \case
          Q3 -> True
          _ -> False
      Q3 -> \case
        Just Zero -> Set $ const False
        Just One -> Set $ \case
          Q4 -> True
          _ -> False
        Nothing -> Set $ const False
      Q4 -> \case
        Just Zero -> Set $ \case
          Q4 -> True
          _ -> False
        Just One -> Set $ \case
          Q4 -> True
          _ -> False
        Nothing -> Set $ const False

example2Tests :: Test
example2Tests =
  TestList
    [ try "nfa example2" True example2 (binarize "111"),
      try "nfa example2" False example2 (binarize "1000"),
      try "nfa example2" True example2 (binarize "010110"),
      try "nfa example2" True example2 (binarize "011"),
      try "dfa example2" True (nfaToDfa example2) (binarize "111"),
      try "dfa example2" False (nfaToDfa example2) (binarize "1000"),
      try "dfa example2" True (nfaToDfa example2) (binarize "010110"),
      try "dfa example2" True (nfaToDfa example2) (binarize "011"),
      tryT "typedNfa example2" True example2T (binarize "111"),
      tryT "typedNfa example2" False example2T (binarize "1000"),
      tryT "typedNfa example2" True example2T (binarize "010110"),
      tryT "typedNfa example2" True example2T (binarize "011"),
      try "typedDfa example2" True (nfaTToDfa example2T) (binarize "111"),
      try "typedDfa example2" False (nfaTToDfa example2T) (binarize "1000"),
      try "typedDfa example2" True (nfaTToDfa example2T) (binarize "010110"),
      try "typedDfa example2" True (nfaTToDfa example2T) (binarize "011")
    ]

main :: IO Counts
main =
  runTestTT $
    TestList
      [ TestLabel "example1" example1Tests,
        TestLabel "example2" example2Tests
      ]
