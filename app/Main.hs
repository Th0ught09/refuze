module Main where

import System.Environment (getArgs)
import System.IO
import System.Random
import Text.Regex.TDFA

pureGen :: StdGen
pureGen = mkStdGen 137

data RTree a = Nil | Node String (RTree a) (RTree a)

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

getLTree :: RTree a -> String
getLTree a = "matches"

getRTree :: RTree a -> String
getRTree a = "non matches"

startTree :: String -> String -> Int -> RTree a
startTree regex string depth = Node string (getNeg regex string depth) (getPos regex string depth)

getNeg :: String -> String -> Int -> RTree a
getNeg regex string depth =
  let newString = badMatch regex string
   in Node newString (getNeg regex newString (depth - 1)) (getPos regex newString (depth - 1))
getNeg _ _ 0 = Nil

getPos :: String -> String -> Int -> RTree a
getPos regex string depth =
  let newString = posMatch regex string
   in Node newString (getNeg regex newString (depth - 1)) (getPos regex newString (depth - 1))
getPos _ _ 0 = Nil

badMatch :: String -> String -> String
badMatch regex string
  | newString =~ regex = badMatch regex string
  | otherwise = newString
  where
    newString = getNewString string

posMatch :: String -> String -> String
posMatch regex string
  | newString =~ regex = newString
  | otherwise = posMatch regex string
  where
    newString = getNewString string

getNewString :: String -> String
getNewString string =
  let choice = fst $ uniformR (1 :: Int, 3) pureGen
   in case choice of
        1 -> removeChar string
        2 -> addChar string
        _ -> changeChar string

removeChar :: String -> String
removeChar string = ""

addChar :: String -> String
addChar string = ""

changeChar :: String -> String
changeChar string = ""

getRanIndex :: String -> (Int, StdGen)
getRanIndex string = uniformR (1 :: Int, length string :: Int) pureGen
