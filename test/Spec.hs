module Main where

import Refuze.Match
import Refuze.String
import System.Random
import Test.Tasty
import Test.Tasty.HUnit

pureGen = mkStdGen 137

refuze_tests :: TestTree
refuze_tests = testGroup "refuze tests" [match_tests]

match_tests :: TestTree
match_tests =
  testGroup
    "Testing Matching library"
    [ testCase "returns empty string" $
        compare (badMatch "" "" (0, pureGen) 0) "" @?= EQ,
      testCase "Basic new string on bad match" $
        compare (badMatch "h" "gq" (1, pureGen) 1) "gSq" @?= EQ
    ]

main :: IO ()
main = defaultMain $ refuze_tests
