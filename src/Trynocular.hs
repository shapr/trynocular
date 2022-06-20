{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Trynocular where

import Control.Applicative (Alternative (..))
import Data.Bifunctor (second)
import Data.Char (chr)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Kind (Type)
import Data.Universe.Helpers ((+*+), (+++))
import Data.Void (Void, absurd)
import Data.Word (Word16, Word32, Word64, Word8)
import System.Random

data Generator :: Type -> Type where
  Empty :: Generator Void
  Trivial :: Generator ()
  Choice :: Generator a -> Generator b -> Generator (Either a b)
  Both :: Generator a -> Generator b -> Generator (a, b)
  Apply :: (a -> b) -> Generator a -> Generator b

-- | fmap id should be id, fmap id here will be Apply id ? which is a different
-- term than the input.  If constructors are not exported, the Functor law holds
-- up to observable behavior.
instance Functor Generator where fmap = Apply

instance Applicative Generator where
  pure x = Apply (const x) Trivial
  a <*> b = Apply (uncurry ($)) (Both a b)

instance Alternative Generator where
  empty = Apply absurd Empty
  a <|> b = Apply (either id id) (Choice a b)

data GenKey = TrivialKey | ChoiceKey Bool GenKey | BothKey GenKey GenKey
  deriving (Eq, Ord, Show)

runGenerator :: Generator a -> IO (Maybe (GenKey, a))
runGenerator Empty = return Nothing
runGenerator Trivial = pure (Just (TrivialKey, ()))
runGenerator (Choice ga gb) = do
  let left = fmap (\(k, x) -> (ChoiceKey True k, Left x)) <$> runGenerator ga
      right = fmap (\(k, x) -> (ChoiceKey False k, Right x)) <$> runGenerator gb
  randomIO >>= \case
    True -> left >>= maybe right (pure . Just)
    False -> right >>= maybe left (pure . Just)
runGenerator (Both ga gb) = do
  left <- runGenerator ga
  right <- runGenerator gb
  case (left, right) of
    (Just (k1, x1), Just (k2, x2)) -> pure (Just (BothKey k1 k2, (x1, x2)))
    _ -> pure Nothing
runGenerator (Apply f ga) = fmap (second f) <$> runGenerator ga

enumGenerator :: Generator a -> [(GenKey, a)]
enumGenerator Empty = []
enumGenerator Trivial = [(TrivialKey, ())]
enumGenerator (Choice ga gb) =
  [(ChoiceKey True k, Left x) | (k, x) <- enumGenerator ga]
    +++ [(ChoiceKey False k, Right x) | (k, x) <- enumGenerator gb]
enumGenerator (Both ga gb) =
  [ (BothKey k1 k2, (x, y))
    | ((k1, x), (k2, y)) <- enumGenerator ga +*+ enumGenerator gb
  ]
enumGenerator (Apply f g) = map (second f) (enumGenerator g)

rerunGenerator :: Generator a -> GenKey -> a
rerunGenerator Trivial TrivialKey = ()
rerunGenerator (Choice ga gb) (ChoiceKey b k)
  | b = Left (rerunGenerator ga k)
  | otherwise = Right (rerunGenerator gb k)
rerunGenerator (Both ga gb) (BothKey k1 k2) =
  (rerunGenerator ga k1, rerunGenerator gb k2)
rerunGenerator (Apply f ga) k = f (rerunGenerator ga k)
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
