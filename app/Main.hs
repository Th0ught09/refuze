module Main where

import System.Environment (getArgs)
import System.Random
import Text.Regex.TDFA

data Node = String

data RTree a = Nil | Node (RTree a) (RTree a)

main :: IO ()
main = do
  a <- randomRIO (1 :: Int, 20)
  print a
  args <- getArgs
  case args of
    [regex, string] -> putStrLn (checkInput regex string)
    _ -> putStrLn "needs 2 arguments"

checkInput :: String -> String -> String
checkInput regex string
  | string =~ regex = do
      "works"
      startTree regex string
  | otherwise = "ensure the string mathces the regex"

startTree :: String -> String -> RTree a
startTree regex string = Node (getNeg regex string) (getPos regex string)

getNeg :: String -> String -> RTree a
getNeg regex string = Node Nil Nil

getPos :: String -> String -> RTree a
getPos regex string = Node Nil Nil
