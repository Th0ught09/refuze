module Refuze.Tree where

import Data.Bits
import Refuze.Match
import Refuze.String
import System.Random

data RTree a = Nil | Node {value :: String, left :: RTree a, right :: RTree a} deriving (Show)

pureGen :: StdGen
pureGen = mkStdGen 137

getLTree :: RTree a -> String
getLTree a = "matches"

getRTree :: RTree a -> String
getRTree a = "non matches"

startTree :: String -> String -> Int -> RTree a
startTree regex string depth = Node string (getNeg regex string depth pureGen) (getPos regex string depth pureGen)

getNeg :: String -> String -> Int -> StdGen -> RTree a
getNeg _ _ 0 _ = Nil
getNeg regex string depth gen =
  let rand = getRanIndex string gen
      new_gen = snd rand
      newString = badMatch regex string rand 1000
   in Node newString (getNeg regex newString (shiftR depth 2) new_gen) (getPos regex newString (shiftR depth 2) new_gen)

getPos :: String -> String -> Int -> StdGen -> RTree a
getPos _ _ 0 _ = Nil
getPos regex string depth gen =
  let rand = getRanIndex string gen
      new_gen = snd rand
      newString = posMatch regex string rand 1000
   in Node newString (getNeg regex newString (shiftR depth 2) new_gen) (getPos regex newString (shiftR depth 2) new_gen)
