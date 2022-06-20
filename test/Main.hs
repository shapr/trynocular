module Main where

import Data.Word (Word8)
import Test.Hspec
import Trynocular

main :: IO ()
main = hspec $ do
  describe "gen" $ do
    it "generates unit values" $ enumerateValues gen `shouldBe` [()]
    it "generates bool values" $ enumerateValues gen `shouldBe` [False, True]
    it "generates words" $
      enumerateValues gen `shouldBe` ([0 .. 255] :: [Word8])
    it "generates lists" $
      take 5 (enumerateValues gen)
        `shouldBe` [[], [()], [(), ()], [(), (), ()], [(), (), (), ()]]
