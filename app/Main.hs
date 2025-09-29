module Main where

import System.Environment (getArgs)
import System.Exit
import System.IO
import System.Random
import Text.Regex.TDFA

pureGen :: StdGen
pureGen = mkStdGen 137

data Node = String

data RTree a = Nil | Node (RTree a) (RTree a)

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
  | string =~ regex = hPutStrLn stderr "ensure the string matches the regex"
  | otherwise = getTreeOutput regex string

getTreeOutput :: String -> String -> IO ()
getTreeOutput regex string =
  let tree = startTree regex string 1000
   in do
        putStrLn $ getLTree tree
        hPutStrLn stderr $ getRTree tree

getLTree :: RTree a -> String
getLTree a = "matches"

getRTree :: RTree a -> String
getRTree a = "non matches"

startTree :: String -> String -> Int -> RTree a
startTree regex string depth = Node (getNeg regex string depth) (getPos regex string depth)

getNeg :: String -> String -> Int -> RTree a
getNeg regex string depth = Node Nil Nil
getNeg regex string 0 = Nil

badMatch :: String -> String -> String
badMatch regex string =
  let index = fst $ uniformR (1 :: Int, length string :: Int) pureGen
      choice = fst $ uniformR (1 :: Int, 3) pureGen
   in case choice of
        1 -> removeChar regex string
        2 -> addChar regex string
        _ -> changeChar regex string

removeChar :: String -> String -> String
removeChar regex string = ""

addChar :: String -> String -> String
addChar regex string = ""

changeChar :: String -> String -> String
changeChar regex string = ""

getPos :: String -> String -> Int -> RTree a
getPos regex string depth = Node Nil Nil
getPos regex string 0 = Nil
