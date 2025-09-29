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
  args <- getArgs
  processArgs args

processArgs :: [String] -> IO ()
processArgs a = case a of
  [regex, string] -> testRegex regex string
  _ -> hPutStrLn stderr "requires 2 arguments"

testRegex :: String -> String -> IO ()
testRegex regex string
  | string =~ regex = hPutStrLn stderr "ensure the string matches the regex"
  | otherwise = getTreeOutput regex string

getTreeOutput :: String -> String -> IO ()
getTreeOutput regex string =
  let tree = startTree regex string
   in do
        putStrLn $ getLTree tree
        hPutStrLn stderr $ getRTree tree

getLTree :: RTree a -> String
getLTree a = "matches"

getRTree :: RTree a -> String
getRTree a = "non matches"

startTree :: String -> String -> RTree a
startTree regex string = Node (getNeg regex string) (getPos regex string)

getNeg :: String -> String -> RTree a
getNeg regex string = Node Nil Nil

getPos :: String -> String -> RTree a
getPos regex string = Node Nil Nil
