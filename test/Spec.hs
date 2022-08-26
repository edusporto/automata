import Automata.Automaton (Automaton (accepts))
import Automata.DFA
import Data.Map (Map, fromList, (!))
import Test.HUnit

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

main :: IO Counts
main =
  runTestTT $
    TestList
      [ TestLabel "example1" example1Tests
      ]
