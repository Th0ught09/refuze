module Refuze.String where

import Data.Char
import System.Random

getNewString :: String -> (Int, StdGen) -> String
getNewString string rand =
  let choice = fst $ uniformR (1 :: Int, 3) (snd rand)
      random_char = chr $ fst $ uniformR (32 :: Int, 126) (snd rand)
   in case choice of
        1 -> removeChar string (fst rand)
        2 -> addChar string (fst rand) random_char
        _ -> changeChar string (fst rand) random_char

removeChar :: String -> Int -> String
removeChar string index =
  let splitUp = splitAt index string
   in fst splitUp ++ tail (snd splitUp)

addChar :: String -> Int -> Char -> String
addChar string index random_char =
  let splitUp = splitAt index string
   in fst splitUp ++ [random_char] ++ snd splitUp

changeChar :: String -> Int -> Char -> String
changeChar string index random_char =
  let splitUp = splitAt index string
   in fst splitUp ++ [random_char] ++ tail (snd splitUp)

getRanIndex :: String -> StdGen -> (Int, StdGen)
getRanIndex string gen = uniformR (1 :: Int, length string :: Int) gen
