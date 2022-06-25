{-# LANGUAGE DataKinds #-}

module Main where

import Data.Int (Int16, Int8)
import Data.Word (Word16, Word8)
import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import StrictUtil (strictCheck)
import Test.Hspec (describe, hspec, it, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary (..), Property, genericShrink, oneof, (===))
import Test.StrictCheck (Consume, Produce (..), Shaped, Spec (..), recur)
import Trynocular (Generable (..), Generator, fromKey, toKey, values, pickKey)
import Control.Monad (replicateM_)

data Foo
  = Foo1 String !Word
  | Foo2 [Integer]
  | Foo3 Foo Foo
  deriving (Generic, Eq, Show)

instance Arbitrary Foo where
  arbitrary =
    oneof
      [ Foo1 <$> arbitrary <*> arbitrary,
        Foo2 <$> arbitrary,
        Foo3 <$> arbitrary <*> arbitrary
      ]
  shrink = genericShrink

instance Produce Foo where
  produce =
    oneof
      [ Foo1 <$> recur <*> recur,
        Foo2 <$> recur,
        Foo3 <$> recur <*> recur
      ]

instance Generable Foo

instance Shaped Foo

instance SOP.Generic Foo

instance SOP.HasDatatypeInfo Foo

instance Consume Foo

main :: IO ()
main = hspec $ do
  describe "gen" $ do
    it "generates unit values" $ values genAny `shouldBe` [()]
    it "generates bool values" $ values genAny `shouldBe` [False, True]
    it "generates words" $
      values genAny `shouldBe` ([0 .. 255] :: [Word8])
    it "generates lists" $
      take 5 (values genAny)
        `shouldBe` [[], [()], [(), ()], [(), (), ()], [(), (), (), ()]]
    it "generates ADTs" $
      take 10 (values genAny)
        `shouldBe` [ Foo1 "" 0,
                     Foo2 [],
                     Foo1 "" 1,
                     Foo3 (Foo1 "" 0) (Foo1 "" 0),
                     Foo1 "\NUL" 0,
                     Foo2 [0],
                     Foo1 "" 2,
                     Foo3 (Foo1 "" 0) (Foo2 []),
                     Foo1 "\NUL" 1,
                     Foo2 [0, 0]
                   ]

    describe "toKey genAny . fromKey genAny == id" $ do
      let equalityTest :: (Generable a, Show a, Eq a) => Generator a -> IO ()
          equalityTest g = do
            k <- pickKey g
            toKey g (fromKey g k) `shouldBe` k

      it "()" $ replicateM_ 1000 $ equalityTest (genAny @())
      it "Int8" $ replicateM_ 1000 $ equalityTest (genAny @Int8)
      it "Word8" $ replicateM_ 1000 $ equalityTest (genAny @Word8)
      it "Int16" $ replicateM_ 1000 $ equalityTest (genAny @Int16)
      it "Word16" $ replicateM_ 1000 $ equalityTest (genAny @Word16)
      it "Maybe Int8" $ replicateM_ 1000 $ equalityTest (genAny @(Maybe Int8))
      it "Char" $ replicateM_ 1000 $ equalityTest (genAny @Char)
      it "[Char]" $ replicateM_ 1000 $ equalityTest (genAny @[Char])
      it "Integer" $ replicateM_ 1000 $ equalityTest (genAny @Integer)
      it "Foo" $ replicateM_ 1000 $ equalityTest (genAny @Foo)

    describe "fromKey genAny . toKey genAny == id" $ do
      let viaKey :: Generable a => a -> a
          viaKey = fromKey genAny . toKey genAny

      describe "check equality" $ do
        let equalityProp :: (Generable a, Show a, Eq a) => a -> Property
            equalityProp = \(x :: a) -> x === viaKey x

        prop "()" $ equalityProp @()
        prop "Int8" $ equalityProp @Int8
        prop "Word8" $ equalityProp @Word8
        prop "Int16" $ equalityProp @Int16
        prop "Word16" $ equalityProp @Word16
        prop "Maybe Int8" $ equalityProp @(Maybe Int8)
        prop "Char" $ equalityProp @Char
        prop "[Char]" $ equalityProp @[Char]
        prop "Integer" $ equalityProp @Integer
        prop "Foo" $ equalityProp @Foo

      describe "check strictness" $ do
        let spec :: Test.StrictCheck.Spec '[a] a
            spec = Spec $ \predict d _x -> predict d

        describe "check that spec describes id" $ do
          it "()" $ strictCheck spec (id @())
          it "Int8" $ strictCheck spec (id @Int)
          it "Maybe Int8" $ strictCheck spec (id @(Maybe Int))
          it "Char" $ strictCheck spec (id @Char)
          it "[Char]" $ strictCheck spec (id @[Char])
          it "Integer" $ strictCheck spec (id @Integer)
          it "Foo" $ strictCheck spec (id @Foo)

        it "()" $ strictCheck spec (viaKey @())
        it "Int8" $ strictCheck spec (viaKey @Int)
        it "Maybe Int8" $ strictCheck spec (viaKey @(Maybe Int))
        it "Char" $ strictCheck spec (viaKey @Char)
        it "[Char]" $ strictCheck spec (viaKey @[Char])
        it "Integer" $ strictCheck spec (viaKey @Integer)
        it "Foo" $ strictCheck spec (viaKey @Foo)
