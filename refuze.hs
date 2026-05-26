module Main where

import Data.Maybe
import Refuze.Tree
import System.Console.GetOpt
import System.Environment
import System.IO
import Text.Regex.TDFA

data Options = Options
  { optOut :: String,
    optErr :: String
  }
  deriving (Show)

data Flag = Flag String String

options =
  [ Option
      "o"
      ["output-file"]
      (OptArg (maybe (Flag "color" "always") (Flag "color")) "WHEN")
      "Output File"
  ]

main :: IO ()
main = do
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
        putStrLn $ getLTree tree []
        hPutStrLn stderr $ getRTree tree []
