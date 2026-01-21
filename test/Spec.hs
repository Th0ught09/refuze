module Main where

import Refuze.Match
import Refuze.String
import System.Random
import Test.Hspec
import Test.Tasty
import Test.Tasty.HUnit
import Tests.Command
import Tests.Match

pureGen = mkStdGen 137

refuze_tests :: TestTree
refuze_tests = testGroup "refuze tests" [match_tests]

match_tests :: TestTree
match_tests =
  testGroup
    "Testing Matching library"
    [ testCase "returns empty string" $
        assertBool "empty string" $
          badMatch "" "" (0, pureGen) 0 == ""
    ]

main :: IO ()
main = defaultMain $ refuze_tests

-- describe "Match" $ do
--   it
