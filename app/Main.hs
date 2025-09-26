module Main where

import System.Environment (getArgs)
import System.Exit
import System.IO
import System.Random
import Text.Regex.TDFA

data Node = String

data RTree a = Nil | Node (RTree a) (RTree a)

main :: IO ()
main = do
  a <- randomRIO (1 :: Int, 20)
  args <- getArguments
  putStrLn "filler"
  hPutStrLn stderr "err"

getArguments = do
  args <- getArgs
  case args of
    [regex, string] -> checkInput regex, string
    _ -> exitWith (ExitFailure 2)

checkInput :: String -> String -> Int
checkInput regex string
  | string =~ regex = 1
  | otherwise = 2

startTree :: String -> String -> String
startTree regex string = do
  -- a <- Node (getNeg regex string) (getPos regex string)
  "hi"

getNeg :: String -> String -> RTree a
getNeg regex string = Node Nil Nil

getPos :: String -> String -> RTree a
getPos regex string = Node Nil Nil
