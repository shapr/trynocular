{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main where

import Control.Exception (evaluate)
import Data.Functor.Identity (Identity (..))
import Data.List (nub, sort)
import Data.Word (Word8)
import GHC.Generics (Generic)
import Test.Hspec (describe, hspec, it, shouldBe, shouldReturn)
import Trynocular
  ( Generable (..),
    Key,
    KeyF (..),
    PartialKey,
    fromKey,
    keys,
    spy,
    totalKey,
    values,
  )

data Foo
  = Foo1 String Word
  | Foo2 [Integer] !Bool
  | Foo3 Foo Foo
  deriving (Generic, Eq, Show)

instance Generable Foo

main :: IO ()
main = hspec $ do
  describe "gen" $ do
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

  describe "observable demand" $ do
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
