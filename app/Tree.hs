module Tree where

import Match
import String
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
getNeg regex string depth gen =
  let rand = getRanIndex string gen
      new_gen = snd rand
      newString = badMatch regex string rand
   in Node newString (getNeg regex newString (depth - 1) new_gen) (getPos regex newString (depth - 1) new_gen)
getNeg _ _ 0 _ = Nil

getPos :: String -> String -> Int -> StdGen -> RTree a
getPos regex string depth gen =
  let rand = getRanIndex string gen
      new_gen = snd rand
      newString = posMatch regex string rand
   in Node newString (getNeg regex newString (depth - 1) new_gen) (getPos regex newString (depth - 1) new_gen)
getPos _ _ 0 _ = Nil
