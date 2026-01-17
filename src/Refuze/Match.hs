module Refuze.Match where

import Refuze.String
import System.Random
import Text.Regex.TDFA

badMatch :: String -> String -> (Int, StdGen) -> Int -> String
badMatch regex string rand 0 = ""
badMatch regex string rand int
  | newString =~ regex = badMatch regex string (getRanIndex string (snd rand)) (int - 1)
  | otherwise = newString
  where
    newString = getNewString string rand

posMatch :: String -> String -> (Int, StdGen) -> Int -> String
posMatch regex string rand 0 = ""
posMatch regex string rand int
  | newString =~ regex = newString
  | otherwise = posMatch regex string (getRanIndex string (snd rand)) (int - 1)
  where
    newString = getNewString string rand

getRanIndex :: String -> StdGen -> (Int, StdGen)
getRanIndex string gen = uniformR (1 :: Int, length string :: Int) gen
