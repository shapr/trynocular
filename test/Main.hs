module Main where

import Data.Word (Word8)
import Test.Hspec
import Trynocular

main :: IO ()
main = hspec $ do
  describe "gen" $ do
    it "generates unit values" $ enumerateValues genAny `shouldBe` [()]
    it "generates bool values" $ enumerateValues genAny `shouldBe` [False, True]
    it "generates words" $
      enumerateValues genAny `shouldBe` ([0 .. 255] :: [Word8])
    it "generates lists" $
      take 5 (enumerateValues genAny)
        `shouldBe` [[], [()], [(), ()], [(), (), ()], [(), (), (), ()]]
