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
  | forall b c. Both (b -> c -> a) (Generator b) (Generator c)

instance Functor Generator where
  fmap _ Empty = Empty
  fmap f (Trivial a) = Trivial (f a)
  fmap f (Choice g1 g2) = Choice (fmap f g1) (fmap f g2)
  fmap f (Both f' g1 g2) = Both (\x y -> f (f' x y)) g1 g2

instance Applicative Generator where
  pure = Trivial
  a <*> b = Both ($) a b

instance Alternative Generator where
  empty = Empty
  (<|>) = Choice

data GenKey = TrivialKey | ChoiceKey Bool GenKey | BothKey GenKey GenKey
  deriving (Eq, Ord, Show)

runGenerator :: Generator a -> IO (Maybe (GenKey, a))
runGenerator Empty = return Nothing
runGenerator (Trivial x) = pure (Just (TrivialKey, x))
runGenerator (Choice ga gb) = do
  let left = fmap (\(k, x) -> (ChoiceKey True k, x)) <$> runGenerator ga
      right = fmap (\(k, x) -> (ChoiceKey False k, x)) <$> runGenerator gb
  randomIO >>= \case
    True -> left >>= maybe right (pure . Just)
    False -> right >>= maybe left (pure . Just)
runGenerator (Both f ga gb) = do
  left <- runGenerator ga
  right <- runGenerator gb
  case (left, right) of
    (Just (k1, x1), Just (k2, x2)) -> pure (Just (BothKey k1 k2, f x1 x2))
    _ -> pure Nothing

enumGenerator :: Generator a -> [(GenKey, a)]
enumGenerator Empty = []
enumGenerator (Trivial x) = [(TrivialKey, x)]
enumGenerator (Choice ga gb) =
  (first (ChoiceKey True) <$> enumGenerator ga)
    +++ (first (ChoiceKey False) <$> enumGenerator gb)
enumGenerator (Both f ga gb) =
  [ (BothKey k1 k2, f x y)
    | ((k1, x), (k2, y)) <- enumGenerator ga +*+ enumGenerator gb
  ]

rerunGenerator :: Generator a -> GenKey -> a
rerunGenerator (Trivial x) TrivialKey = x
rerunGenerator (Choice ga gb) (ChoiceKey b k)
  | b = rerunGenerator ga k
  | otherwise = rerunGenerator gb k
rerunGenerator (Both f ga gb) (BothKey k1 k2) =
  f (rerunGenerator ga k1) (rerunGenerator gb k2)
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
