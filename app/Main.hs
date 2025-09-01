module Main where

import Text.Regex.TDFA

emailRegex = "[a-zA-Z0-9+._-]+@[a-zA-Z-]+\\.[a-z]+"

main :: IO ()
main
    | "my email is email@email.com" =~ emailRegex = putStrLn "Hello, Haskell!"
    | otherwise = putStrLn "failed"
