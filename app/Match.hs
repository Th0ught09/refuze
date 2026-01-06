module Match where

import String
import System.Random
import Text.Regex.TDFA

badMatch :: String -> String -> (Int, StdGen) -> String
badMatch regex string rand
  | newString =~ regex = badMatch regex string rand
  | otherwise = newString
  where
    newString = getNewString string rand

posMatch :: String -> String -> (Int, StdGen) -> String
posMatch regex string rand
  | newString =~ regex = newString
  | otherwise = posMatch regex string rand
  where
    newString = getNewString string rand
