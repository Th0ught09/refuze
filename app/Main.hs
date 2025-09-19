module Main where

import System.Environment (getArgs)
import Text.Regex.TDFA
import System.Random

emailRegex = "[a-zA-Z0-9+._-]+@[a-zA-Z-]+\\.[a-z]+"

main :: IO ()
main = do
  g <- getStdGen
  print $ take 10 (randomRs ('a', 'z') g)
  args <- getArgs
  case args of
    [regex, string] -> putStrLn (checkInput regex string)
    _ -> putStrLn "needs 2 arguments"

checkInput :: String -> String -> String
checkInput regex string
  | string =~ regex = " works!"
  | otherwise = "ensure the string mathces the regex"
