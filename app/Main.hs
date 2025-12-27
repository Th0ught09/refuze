module Main where

import Data.Char
import System.Environment (getArgs)
import System.IO
import System.Random
import Text.Regex.TDFA

pureGen :: StdGen
pureGen = mkStdGen 137

data RTree a = Nil | Node {value :: String, left :: RTree a, right :: RTree a} deriving (Show)

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
startTree regex string depth = Node string (getNeg regex string depth pureGen) (getPos regex string depth pureGen)

getNeg :: String -> String -> Int -> StdGen -> RTree a
getNeg regex string depth gen =
  let rand = getRanIndex string gen
      new_gen = snd rand
      newString = badMatch regex string rand
   in Node newString (getNeg regex newString (depth - 1) new_gen) (getPos regex newString (depth - 1) new_gen)
getNeg _ _ 0 _ = Nil

getPos :: String -> String -> Int -> StdGen -> RTree a
getPos regex string depth gen =
  let rand = getRanIndex string gen
      new_gen = snd rand
      newString = posMatch regex string rand
   in Node newString (getNeg regex newString (depth - 1) new_gen) (getPos regex newString (depth - 1) new_gen)
getPos _ _ 0 _ = Nil

badMatch :: String -> String -> (Int, StdGen) -> String
badMatch regex string rand
  | newString =~ regex = badMatch regex string rand
  | otherwise = newString
  where
    newString = getNewString string rand

posMatch :: String -> String -> (Int, StdGen) -> String
posMatch regex string rand
  | newString =~ regex = newString
  | otherwise = posMatch regex string rand
  where
    newString = getNewString string rand

getNewString :: String -> (Int, StdGen) -> String
getNewString string rand =
  let choice = fst $ uniformR (1 :: Int, 3) (snd rand)
   in case choice of
        1 -> removeChar string (fst rand)
        2 -> addChar string (fst rand)
        _ -> changeChar string (fst rand)

removeChar :: String -> Int -> String
removeChar string index =
  let splitUp = splitAt index string
   in fst splitUp ++ tail (snd splitUp)

addChar :: String -> Int -> String
addChar string index =
  let char = chr $ fst $ uniformR (32, 126) pureGen
      splitUp = splitAt index string
   in fst splitUp ++ [char] ++ snd splitUp

changeChar :: String -> Int -> String
changeChar string index =
  let char = chr $ fst $ uniformR (32, 126) pureGen
      splitUp = splitAt index string
   in fst splitUp ++ [char] ++ tail (snd splitUp)

getRanIndex :: String -> StdGen -> (Int, StdGen)
getRanIndex string gen = uniformR (1 :: Int, length string :: Int) gen
