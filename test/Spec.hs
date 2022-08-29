{-# LANGUAGE LambdaCase #-}

import Automata.Automaton (Automaton (accepts))
import Automata.DFA
import Automata.NFA
import Data.Map (Map, fromList, (!))
import Test.HUnit

data Binary = Zero | One
  deriving (Eq, Show)

example1 :: DFA String Char
example1 =
  DFA
    (\q a -> transitions ! q ! a)
    "q"
    (== "q001")
  where
    transitions :: Map String (Map Char String)
    transitions =
      fromList
        [ ("q", fromList [('0', "q0"), ('1', "q")]),
          ("q0", fromList [('0', "q00"), ('1', "q")]),
          ("q00", fromList [('0', "q00"), ('1', "q001")]),
          ("q001", fromList [('0', "q001"), ('1', "q001")])
        ]

example1Tests :: Test
example1Tests =
  TestList
    [ TestCase (assert (accepts example1 "001")),
      TestCase (assert (accepts example1 "0010")),
      TestCase (assert (accepts example1 "0101010100100")),
      TestCase (assert (not (accepts example1 "10000000000")))
    ]

data Example2States = Q1 | Q2 | Q3 | Q4
  deriving (Eq, Show)

example2 :: NFA Example2States Binary
example2 =
  NFA
    f
    Q1
    (== Q4)
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

binarize :: [Char] -> [Binary]
binarize =
  map
    ( \case
        '0' -> Zero
        '1' -> One
        _ -> error "invalid binary digit"
    )

example2Tests :: Test
example2Tests =
  TestList
    [ TestCase $ assert (accepts example2 (binarize "111")),
      TestCase $ assert $ not (accepts example2 (binarize "1000")),
      TestCase $ assert (accepts example2 (binarize "010110")),
      TestCase $ assert (accepts example2 (binarize "011"))
    ]

main :: IO Counts
main =
  runTestTT $
    TestList
      [ TestLabel "example1" example1Tests,
        TestLabel "example2" example2Tests
      ]
