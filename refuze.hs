module Main where

import Data.Char
import Refuze.Tree
import System.Environment (getArgs)
import System.IO
import System.Random
import Text.Regex.TDFA

main :: IO ()
main = do
  -- a <- randomRIO (1 :: Int, 20)
  args <- getArgs
  processArgs args

processArgs :: [String] -> IO ()
processArgs a = case a of
  [regex, string] -> testRegex regex string
  _ -> hPutStrLn stderr "requires 2 arguments"

testRegex :: String -> String -> IO ()
testRegex regex string
  | string =~ regex = getTreeOutput regex string
  | otherwise = hPutStrLn stderr "ensure the string matches the regex"

getTreeOutput :: String -> String -> IO ()
getTreeOutput regex string =
  let tree = startTree regex string 10
   in do
        putStrLn $ getLTree tree
        hPutStrLn stderr $ getRTree tree
