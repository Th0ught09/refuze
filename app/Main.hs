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

-- getOutput :: Either (RTree a) String -> String
-- getOutput tree = "matched"
-- getOutput = "no match"
-- getArguments = do
--   args <- getArgs
--   case args of
--     [regex, string] -> checkInput regex string
--     _ -> exitWith (ExitFailure 2)
-- checkInput :: String -> String -> Int
-- checkInput regex string
--   | string =~ regex = 1
--   | otherwise = exitWith (ExitFailure 1)
processTree :: Int -> String
processTree 1 = "does not work"
processTree 2 = "no match"
processTree 0 = "works!"
processTree a = "other"

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

processArgs :: [String] -> IO ()
processArgs a = case a of
  [regex, string] -> testRegex regex string
  _ -> hPutStrLn stderr "requires 2 arguments"

testRegex :: String -> String -> IO ()
testRegex regex string
  | string =~ regex = hPutStrLn stderr "ensure the string matches the regex"
  | otherwise = getTreeOutput regex string

startTree :: String -> String -> RTree a
startTree regex string = Node (getNeg regex string) (getPos regex string)

getNeg :: String -> String -> RTree a
getNeg regex string = Node Nil Nil

getPos :: String -> String -> RTree a
getPos regex string = Node Nil Nil
