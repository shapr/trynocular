module Main where

import Data.Word (Word8)
import Test.Hspec
import Trynocular

main :: IO ()
main = hspec $ do
  describe "gen" $ do
    it "generates unit values" $
      snd <$> enumGenerator gen `shouldBe` [()]
    it "generates bool values" $
      snd <$> enumGenerator gen `shouldBe` [False, True]
    it "generates words" $
      snd <$> enumGenerator gen `shouldBe` ([0 .. 255] :: [Word8])
    it "generates lists" $
      take 5 (snd <$> enumGenerator gen)
        `shouldBe` [[], [()], [(), ()], [(), (), ()], [(), (), (), ()]]
