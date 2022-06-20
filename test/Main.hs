module Main where

import Data.Word (Word8)
import GHC.Generics (Generic)
import Test.Hspec
import Trynocular

data Foo
  = Foo1 String Word8
  | Foo2 [Integer]
  deriving (Generic, Eq, Show)

instance Generable Foo

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
    it "generates ADTs" $
      take 10 (enumerateValues genAny)
        `shouldBe` [ Foo1 "" 0,
                     Foo2 [],
                     Foo1 "" 1,
                     Foo2 [0],
                     Foo1 "\NUL" 0,
                     Foo2 [0, 0],
                     Foo1 "" 2,
                     Foo2 [-1],
                     Foo1 "\NUL" 1,
                     Foo2 [0, 0, 0]
                   ]
