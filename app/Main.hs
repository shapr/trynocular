{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Applicative (Alternative (..))
import Data.Bifunctor (first)
import Data.Char (chr)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Universe.Helpers ((+*+), (+++))
import Data.Word (Word16, Word32, Word64, Word8)
import System.Random

main :: IO ()
main = putStrLn "Hello, Haskell!"

data Generator a
  = Empty
  | Trivial a
  | Choice (Generator a) (Generator a)
  | forall b. Both (Generator (b -> a)) (Generator b)

instance Functor Generator where
  fmap _ Empty = Empty
  fmap f (Trivial a) = Trivial (f a)
  fmap f (Choice g1 g2) = Choice (fmap f g1) (fmap f g2)
  fmap f (Both g1 g2) = Both ((f .) <$> g1) g2

instance Applicative Generator where
  pure = Trivial
  (<*>) = Both

instance Alternative Generator where
  empty = Empty
  (<|>) = Choice

data GenKey = TrivialKey | ChoiceKey Bool GenKey | BothKey GenKey GenKey
  deriving (Eq, Ord, Show)

runGenerator :: Generator a -> IO (Maybe (GenKey, a))
runGenerator Empty = return Nothing
runGenerator (Trivial x) = pure (Just (TrivialKey, x))
runGenerator (Choice a b) = do
  let left = fmap (\(k, x) -> (ChoiceKey True k, x)) <$> runGenerator a
      right = fmap (\(k, x) -> (ChoiceKey False k, x)) <$> runGenerator b
  randomIO >>= \case
    True -> left >>= maybe right (pure . Just)
    False -> right >>= maybe left (pure . Just)
runGenerator (Both a b) = do
  left <- runGenerator a
  right <- runGenerator b
  case (left, right) of
    (Just (k1, f), Just (k2, x)) -> pure (Just (BothKey k1 k2, f x))
    _ -> pure Nothing

enumGenerator :: Generator a -> [(GenKey, a)]
enumGenerator Empty = []
enumGenerator (Trivial x) = [(TrivialKey, x)]
enumGenerator (Choice a b) =
  (first (ChoiceKey True) <$> enumGenerator a)
    +++ (first (ChoiceKey False) <$> enumGenerator b)
enumGenerator (Both a b) =
  [ (BothKey k1 k2, f x)
    | ((k1, f), (k2, x)) <- enumGenerator a +*+ enumGenerator b
  ]

rerunGenerator :: Generator a -> GenKey -> a
rerunGenerator (Trivial x) TrivialKey = x
rerunGenerator (Choice a b) (ChoiceKey goLeft k)
  | goLeft = rerunGenerator a k
  | otherwise = rerunGenerator b k
rerunGenerator (Both a b) (BothKey k1 k2) =
  (rerunGenerator a k1) (rerunGenerator b k2)
rerunGenerator _ _ = error "key doesn't match generator"

genBits :: Integral t => Int -> Generator t
genBits 0 = pure 0
genBits n = (\x b -> 2 * x + b) <$> genBits (n - 1) <*> (pure 0 <|> pure 1)

genPositive :: Generator Integer
genPositive =
  pure 1
    <|> (* 2) <$> genPositive
    <|> (\n -> 2 * n + 1) <$> genPositive

class Generable a where gen :: Generator a

instance Generable () where gen = pure ()

instance Generable Bool where gen = pure False <|> pure True

instance Generable Char where
  gen = chr <$> (genBits 20 <|> (+ 0x100000) <$> genBits 16)

instance Generable Word8 where gen = genBits 8

instance Generable Word16 where gen = genBits 16

instance Generable Word32 where gen = genBits 32

instance Generable Word64 where gen = genBits 64

instance Generable Int8 where gen = genBits 8

instance Generable Int16 where gen = genBits 16

instance Generable Int32 where gen = genBits 32

instance Generable Int64 where gen = genBits 64

instance Generable Integer where
  gen = pure 0 <|> genPositive <|> negate <$> genPositive

instance Generable a => Generable (Maybe a) where
  gen = pure Nothing <|> Just <$> gen

instance (Generable a, Generable b) => Generable (Either a b) where
  gen = Left <$> gen <|> Right <$> gen

instance (Generable a, Generable b) => Generable (a, b) where
  gen = (,) <$> gen <*> gen

instance Generable a => Generable [a] where
  gen = pure [] <|> ((:) <$> gen <*> gen)
