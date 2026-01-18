module Refuze.Tree where

import Data.Bits
import Refuze.Match
import Refuze.String
import System.Random

data RTree a = Nil | Node {value :: String, left :: RTree a, right :: RTree a} deriving (Show)

pureGen :: StdGen
pureGen = mkStdGen 137

getValue :: RTree a -> String
getValue Nil = ""
getValue a = value a ++ "\n"

getLTree :: RTree a -> [Char] -> String
getLTree Nil b = b
getLTree a b = b ++ getValue (left a) ++ getLTree (left a) b ++ getLTree (right a) b

getRTree :: RTree a -> [Char] -> String
getRTree Nil b = b
getRTree a b = b ++ getValue (right a) ++ getRTree (left a) b ++ getRTree (right a) b

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
