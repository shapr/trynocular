{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Control.Exception (evaluate)
import Data.Foldable (foldl')
import Data.Functor.Identity (Identity (..))
import Data.List (nub, sort)
import Data.Word (Word8)
import GHC.Generics (Generic)
import System.Random (mkStdGen)
import System.Random.Shuffle (shuffle')
import Test.Hspec (Spec, describe, example, hspec, it, shouldBe, shouldReturn, shouldSatisfy)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary (..), Gen, elements, forAll, genericShrink, listOf, oneof, (===))
import Trynocular.Generable (Generable (..))
import Trynocular.Generator (Generator, fromKey, keys, values)
import Trynocular.Key (Key, KeyF (..), PartialKey, partialKeys, spy, subsumes, totalKey)
import Trynocular.PartialKeySet qualified as PartialKeySet
import Trynocular.Quantiler (Quantiler (..), betaQuantiler, emptyCompleteQuantiler, normalQuantiler)

data Foo
  = Foo1 String Word
  | Foo2 [Integer] !Bool
  | Foo3 Foo Foo
  deriving (Generic, Eq, Show)

instance Generable Foo

generatorSpec :: Spec
generatorSpec = do
  it "generates unit values" $ values genAny `shouldBe` [()]

  it "generates bool values" $ values genAny `shouldBe` [False, True]

  it "generates words" $
    sort (values genAny) `shouldBe` ([0 .. 255] :: [Word8])

  it "generates lists" $
    take 5 (values genAny)
      `shouldBe` [[], [()], [(), ()], [(), (), ()], [(), (), (), ()]]

  it "generates ADTs" $ do
    let isFoo1 x = case x of (Foo1 _ _) -> True; _ -> False
        isFoo2 x = case x of (Foo2 _ _) -> True; _ -> False
        isFoo3 x = case x of (Foo3 _ _) -> True; _ -> False
        vals = take 100 (values genAny)

    any isFoo1 vals `shouldBe` True
    any isFoo2 vals `shouldBe` True
    any isFoo3 vals `shouldBe` True
    nub vals `shouldBe` vals

spySpec :: Spec
spySpec = do
  let getDemand :: forall a b. Generable a => Key -> (a -> b) -> IO PartialKey
      getDemand origKey f =
        fst <$> spy origKey (\key -> evaluate (f (fromKey genAny key)))

  describe "Just ()" $ do
    let key = Identity (RightF (Identity TrivialF))

    it "has the correct key" $ do
      fromKey genAny key `shouldBe` Just ()

    -- Demand lattice:
    --
    --     Just ()
    --
    --        |
    --
    --     Just ⊥
    --
    --        |
    --
    --        ⊥

    it "reports demand" $ do
      getDemand @(Maybe ()) key (\_ -> ()) `shouldReturn` Nothing

    it "reports demand" $ do
      getDemand @(Maybe ()) key (\(Just _) -> ())
        `shouldReturn` Just (RightF Nothing)

    it "reports demand" $ do
      getDemand @(Maybe ()) key (\(Just ()) -> ())
        `shouldReturn` Just (RightF (Just TrivialF))

  describe "ADT" $ do
    let strKey = head . keys $ (genAny @String)
    let wordKey = head . keys $ (genAny @Word)
    let integerListKey = head . keys $ (genAny @[Integer])
    let boolKey = head . keys $ (genAny @Bool)
    let subFooKey = head . keys $ (genAny @Foo)

    describe "Foo1 \"\" 0" $ do
      let key = Identity (LeftF (Identity (BothF strKey wordKey)))

      it "has the correct key" $ do
        fromKey genAny key `shouldBe` Foo1 "" 0

      -- Demand lattice:
      --
      --         Foo1 "" 0
      --
      --        /         \
      --       /           \
      --
      --  Foo1 ⊥ 0      Foo1 "" ⊥
      --
      --       \           /
      --        \         /
      --
      --         Foo1 ⊥ ⊥
      --
      --             |
      --             |
      --
      --             ⊥

      it "reports demand" $ do
        getDemand @Foo key (\_ -> ()) `shouldReturn` Nothing

      it "reports demand" $ do
        getDemand @Foo key (\(Foo1 _ _) -> ())
          `shouldReturn` Just
            (LeftF (Just (BothF Nothing Nothing)))

      it "reports demand" $ do
        getDemand @Foo key (\(Foo1 "" _) -> ())
          `shouldReturn` Just
            (LeftF (Just (BothF (totalKey strKey) Nothing)))

      it "reports demand" $ do
        getDemand @Foo key (\(Foo1 _ 0) -> ())
          `shouldReturn` Just
            (LeftF (Just (BothF Nothing (totalKey wordKey))))

      it "reports demand" $ do
        getDemand @Foo key (\(Foo1 "" 0) -> ())
          `shouldReturn` Just
            (LeftF (Just (BothF (totalKey strKey) (totalKey wordKey))))

    describe "Foo2 []" $ do
      let key =
            Identity
              ( RightF
                  (Identity (LeftF (Identity (BothF integerListKey boolKey))))
              )

      it "has the correct key" $ do
        fromKey genAny key `shouldBe` Foo2 [] False

      -- Demand lattice (the Bool field is strict!):
      --
      --        Foo2 [] False
      --
      --             |
      --             |
      --
      --        Foo2 ⊥ False
      --
      --             |
      --             |
      --
      --             ⊥

      it "reports demand" $ do
        getDemand @Foo key (\_ -> ()) `shouldReturn` Nothing

      it "reports demand" $ do
        getDemand @Foo key (\(Foo2 _ _) -> ())
          `shouldReturn` Just
            ( RightF
                ( Just
                    ( LeftF
                        ( Just
                            ( BothF
                                Nothing
                                (totalKey boolKey) -- Strict field
                            )
                        )
                    )
                )
            )

      it "reports demand" $ do
        getDemand @Foo key (\(Foo2 [] _) -> ())
          `shouldReturn` Just
            ( RightF
                ( Just
                    ( LeftF
                        ( Just
                            ( BothF
                                (totalKey integerListKey)
                                (totalKey boolKey) -- Strict field
                            )
                        )
                    )
                )
            )

    describe "Foo3 (Foo1 [] 0) (Foo1 [] 0)" $ do
      let key =
            Identity
              ( RightF
                  (Identity (RightF (Identity (BothF subFooKey subFooKey))))
              )

      it "has the correct key" $ do
        fromKey genAny key `shouldBe` Foo3 (Foo1 "" 0) (Foo1 "" 0)

  describe "[(), ()]" $ do
    let nil f = f (LeftF (f TrivialF))
    let cons f x xs = f (RightF (f (BothF x xs)))
    let unit f = f TrivialF

    let key f = cons f (unit f) (cons f (unit f) (nil f))

    it "has the correct key" $ do
      fromKey genAny (key Identity) `shouldBe` [(), ()]

    -- Demand lattice:
    --
    --                           () : () : []
    --
    --                         /       |       \
    --                        /        |        \
    --
    --          ⊥ : () : []       () : ⊥ : []      () : () : ⊥
    --
    --               |       \ /       |       \ /       |
    --               |        X        |        X        |
    --               |       / \       |       / \       |
    --
    --          ⊥ : ⊥ : []        ⊥ : () : ⊥       () : ⊥ : ⊥
    --
    --                       \         |         /       |
    --                        \        |        /        |
    --                         \       |       /         |
    --                          \      |      /          |
    --
    --                             ⊥ : ⊥ : ⊥          () : ⊥
    --
    --                                        \       /
    --                                         \     /
    --                                          \   /
    --
    --                                          ⊥ : ⊥
    --
    --                                            |
    --                                            |
    --
    --                                            ⊥

    it "reports demand" $ do
      getDemand @[()] (key Identity) (\_ -> ())
        `shouldReturn` Nothing

    it "reports demand" $ do
      getDemand @[()] (key Identity) (\(_ : _) -> ())
        `shouldReturn` cons Just Nothing Nothing

    it "reports demand" $ do
      getDemand @[()] (key Identity) (\(_ : _ : _) -> ())
        `shouldReturn` cons Just Nothing (cons Just Nothing Nothing)

    it "reports demand" $ do
      getDemand @[()] (key Identity) (\(() : _) -> ())
        `shouldReturn` cons Just (unit Just) Nothing

    it "reports demand" $ do
      getDemand @[()] (key Identity) (\(_ : _ : []) -> ())
        `shouldReturn` cons Just Nothing (cons Just Nothing (nil Just))

    it "reports demand" $ do
      getDemand @[()] (key Identity) (\(_ : () : _) -> ())
        `shouldReturn` cons Just Nothing (cons Just (unit Just) Nothing)

    it "reports demand" $ do
      getDemand @[()] (key Identity) (\(() : _ : _) -> ())
        `shouldReturn` cons Just (unit Just) (cons Just Nothing Nothing)

    it "reports demand" $ do
      getDemand @[()] (key Identity) (\(() : () : _) -> ())
        `shouldReturn` cons Just (unit Just) (cons Just (unit Just) Nothing)

    it "reports demand" $ do
      getDemand @[()] (key Identity) (\(() : _ : []) -> ())
        `shouldReturn` cons Just (unit Just) (cons Just Nothing (nil Just))

    it "reports demand" $ do
      getDemand @[()] (key Identity) (\(_ : () : []) -> ())
        `shouldReturn` cons Just Nothing (cons Just (unit Just) (nil Just))

    it "reports demand" $ do
      getDemand @[()] (key Identity) (\(() : () : []) -> ())
        `shouldReturn` key Just

instance Arbitrary (KeyF Identity) where
  arbitrary =
    oneof
      [ pure TrivialF,
        LeftF <$> arbitrary,
        RightF <$> arbitrary,
        BothF <$> arbitrary <*> arbitrary
      ]
  shrink = genericShrink

instance Arbitrary (KeyF Maybe) where
  arbitrary =
    oneof
      [ pure TrivialF,
        LeftF <$> arbitrary,
        RightF <$> arbitrary,
        BothF <$> arbitrary <*> arbitrary
      ]
  shrink = genericShrink

oneOfInfinitelyMany :: [a] -> Gen a
oneOfInfinitelyMany [] = error "oneOfInfinitelyMany: empty list"
oneOfInfinitelyMany [x] = pure x
oneOfInfinitelyMany (x : xs) = oneof [pure x, oneOfInfinitelyMany xs]

compatibleKeys :: Generator a -> Gen Key
compatibleKeys g = oneOfInfinitelyMany (keys g)

partialKeySetSpec :: Spec
partialKeySetSpec = do
  prop "contains nothing in an empty set" $
    \k -> k `PartialKeySet.member` PartialKeySet.empty === False

  it "contains an exact key" $ do
    let k =
          Identity
            ( BothF
                (Identity (LeftF (Identity TrivialF)))
                (Identity (RightF (Identity TrivialF)))
            )
    let k2 =
          Identity
            ( BothF
                (Identity (RightF (Identity TrivialF)))
                (Identity (RightF (Identity TrivialF)))
            )
    let k3 =
          Identity
            ( BothF
                (Identity (LeftF (Identity TrivialF)))
                (Identity (LeftF (Identity TrivialF)))
            )
    let ks = PartialKeySet.singleton (totalKey k)

    k `PartialKeySet.member` ks `shouldBe` True
    k2 `PartialKeySet.member` ks `shouldBe` False
    k3 `PartialKeySet.member` ks `shouldBe` False

  prop "contains any exact key" $ \(k1 :: Key) (k2 :: Key) ->
    k2 `PartialKeySet.member` PartialKeySet.singleton (totalKey k1)
      === (k1 == k2)

  prop "contains a subsumed key" $
    let k =
          Identity
            ( BothF
                (Identity (LeftF (Identity TrivialF)))
                (Identity (RightF (Identity TrivialF)))
            )
     in forAll (elements (partialKeys k)) $ \pk ->
          k `PartialKeySet.member` PartialKeySet.singleton pk

  prop "contains any subsumed key" $ \(pk :: PartialKey) (k :: Key) ->
    k `PartialKeySet.member` PartialKeySet.singleton pk
      `shouldBe` pk `subsumes` k

  prop "contains any multiple exact keys" $
    let g = genAny :: Generator [Bool]
     in forAll (listOf (compatibleKeys g)) $ \ks ->
          forAll (compatibleKeys g) $ \k ->
            k `PartialKeySet.member` PartialKeySet.fromList (totalKey <$> ks)
              === (k `elem` ks)

quantilerSpec :: Spec
quantilerSpec = do
  describe "NormalQuantiler" $ do
    it "estimates normal distributions based on z score" $ do
      -- Responsiveness of 0 ensures the distribution isn't updated.
      let standardQuantiler = normalQuantiler 0 1 0

      snd (quantile standardQuantiler 0) `shouldBe` 0.5
      snd (quantile standardQuantiler 1) `shouldBe` 0.8413447460685429
      snd (quantile standardQuantiler (-1)) `shouldBe` 0.15865525393145707

  describe "BetaQuantiler" $ do
    it "estimates a uniform distribution" $ do
      -- Uniform distribution has a variance of 1/12.
      -- Responsiveness of 0 ensures the distribution isn't updated.
      let uniformQuantiler = betaQuantiler (0, 1) 0.5 (1 / 12) 0
      snd (quantile uniformQuantiler 0.00) `shouldBe` 0.00
      snd (quantile uniformQuantiler 0.25) `shouldBe` 0.25
      snd (quantile uniformQuantiler 0.50) `shouldBe` 0.50
      snd (quantile uniformQuantiler 0.75) `shouldBe` 0.75
      snd (quantile uniformQuantiler 1.00) `shouldBe` 1.00

    it "learns non-zero discrete component at the bottom" $ do
      let initialQuantiler = betaQuantiler (0, 1) 0.5 (1 / 12) 0.1
          trainedQuantiler =
            foldl' ((fst .) . quantile) initialQuantiler (replicate 100 0)
      abs (snd (quantile trainedQuantiler 0) - 0.5) `shouldSatisfy` (< 0.001)

    it "learns non-zero discrete component at the top" $ do
      let initialQuantiler = betaQuantiler (0, 1) 0.5 (1 / 12) 0.1
          trainedQuantiler =
            foldl' ((fst .) . quantile) initialQuantiler (replicate 100 1)
      abs (snd (quantile trainedQuantiler 1) - 0.5) `shouldSatisfy` (< 0.001)

  describe "CompleteQuantiler" $ do
    it "approximates a uniform distribution" $
      example $ do
        let input = shuffle' [1 :: Int .. 10000] 10000 (mkStdGen 123)
            quantiler =
              foldl'
                (\q x -> fst (quantile q x))
                (emptyCompleteQuantiler 0.05)
                input

        -- Accuracy isn't great here, both because the test modifies the
        -- distribution and because we're not generating a lot of values.
        abs (snd (quantile quantiler 0) - 0.00) `shouldSatisfy` (< 0.1)
        abs (snd (quantile quantiler 2500) - 0.25) `shouldSatisfy` (< 0.1)
        abs (snd (quantile quantiler 5000) - 0.50) `shouldSatisfy` (< 0.1)
        abs (snd (quantile quantiler 7500) - 0.75) `shouldSatisfy` (< 0.1)
        abs (snd (quantile quantiler 10000) - 1.00) `shouldSatisfy` (< 0.1)

main :: IO ()
main = hspec $ do
  describe "Generator" generatorSpec
  describe "spy" spySpec
  describe "PartialKeySet" partialKeySetSpec
  describe "Quantiler" quantilerSpec
