module Main where

import qualified String
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Prelude.head" $ do
    it "returns first element of a list" $ do
      head [23 ..] `shouldBe` (23 :: Int)
  describe "String" $ do
    it "Removes a char" $ do
      removeChar "hello" 2 `shouldBe` "helo"
