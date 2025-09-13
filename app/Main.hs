module Main where

import System.Environment (getArgs)
import Text.Regex.TDFA

emailRegex = "[a-zA-Z0-9+._-]+@[a-zA-Z-]+\\.[a-z]+"

main :: IO ()
main = do
  args <- getArgs
  case args of
    [regex, string] -> checkInput regex string
    _ -> putStrLn "needs 2 arguments"

checkInput regex string
  | string =~ regex = putStrLn " works!"
  | otherwise = putStrLn "ensure the string mathces the regex"
