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
import Trynocular (Generable (..), Generator, fromGenKey, toGenKey, values, pickGenKey)

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

    describe "toGenKey genAny . fromGenKey genAny == id" $ do
      let equalityTest :: (Generable a, Show a, Eq a) => Generator a -> IO ()
          equalityTest g = do
            k <- pickGenKey g
            toGenKey g (fromGenKey g k) `shouldBe` k

      it "()" $ equalityTest (genAny @())
      it "Int8" $ equalityTest (genAny @Int8)
      it "Word8" $ equalityTest (genAny @Word8)
      it "Int16" $ equalityTest (genAny @Int16)
      it "Word16" $ equalityTest (genAny @Word16)
      it "Maybe Int8" $ equalityTest (genAny @(Maybe Int8))
      it "Char" $ equalityTest (genAny @Char)
      it "[Char]" $ equalityTest (genAny @[Char])
      it "Integer" $ equalityTest (genAny @Integer)
      it "Foo" $ equalityTest (genAny @Foo)

    describe "fromGenKey genAny . toGenKey genAny == id" $ do
      let viaGenKey :: Generable a => a -> a
          viaGenKey = fromGenKey genAny . toGenKey genAny

      describe "check equality" $ do
        let equalityProp :: (Generable a, Show a, Eq a) => a -> Property
            equalityProp = \(x :: a) -> x === viaGenKey x

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

        it "()" $ strictCheck spec (viaGenKey @())
        it "Int8" $ strictCheck spec (viaGenKey @Int)
        it "Maybe Int8" $ strictCheck spec (viaGenKey @(Maybe Int))
        it "Char" $ strictCheck spec (viaGenKey @Char)
        it "[Char]" $ strictCheck spec (viaGenKey @[Char])
        it "Integer" $ strictCheck spec (viaGenKey @Integer)
        it "Foo" $ strictCheck spec (viaGenKey @Foo)
