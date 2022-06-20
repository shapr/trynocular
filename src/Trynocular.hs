{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Trynocular where

import Control.Applicative (Alternative (..))
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

generateKey :: Generator a -> IO (Maybe GenKey)
generateKey Empty = return Nothing
generateKey Trivial = pure (Just TrivialKey)
generateKey (Choice ga gb) = do
  let left = fmap (ChoiceKey True) <$> generateKey ga
      right = fmap (ChoiceKey False) <$> generateKey gb
  randomIO >>= \case
    True -> left >>= maybe right (pure . Just)
    False -> right >>= maybe left (pure . Just)
generateKey (Both ga gb) = do
  left <- generateKey ga
  right <- generateKey gb
  case (left, right) of
    (Just k1, Just k2) -> pure (Just (BothKey k1 k2))
    _ -> pure Nothing
generateKey (Apply _ ga) = generateKey ga

enumerateKeys :: Generator a -> [GenKey]
enumerateKeys Empty = []
enumerateKeys Trivial = [TrivialKey]
enumerateKeys (Choice ga gb) =
  (ChoiceKey True <$> enumerateKeys ga)
    +++ (ChoiceKey False <$> enumerateKeys gb)
enumerateKeys (Both ga gb) =
  uncurry BothKey <$> enumerateKeys ga +*+ enumerateKeys gb
enumerateKeys (Apply _ g) = enumerateKeys g

generateFromKey :: Generator a -> GenKey -> a
generateFromKey Trivial TrivialKey = ()
generateFromKey (Choice ga gb) (ChoiceKey b k)
  | b = Left (generateFromKey ga k)
  | otherwise = Right (generateFromKey gb k)
generateFromKey (Both ga gb) (BothKey k1 k2) =
  (generateFromKey ga k1, generateFromKey gb k2)
generateFromKey (Apply f ga) k = f (generateFromKey ga k)
generateFromKey _ _ = error "key doesn't match generator"

generateValue :: Generator a -> IO (Maybe a)
generateValue g = fmap (generateFromKey g) <$> generateKey g

enumerateValues :: Generator a -> [a]
enumerateValues g = generateFromKey g <$> enumerateKeys g

genBits :: Integral t => Int -> Generator t
genBits 0 = pure 0
genBits n = (\x b -> 2 * x + b) <$> genBits (n - 1) <*> (pure 0 <|> pure 1)

genPositive :: Generator Integer
genPositive =
  pure 1
    <|> (* 2) <$> genPositive
    <|> (\n -> 2 * n + 1) <$> genPositive

class Generable a where genAny :: Generator a

instance Generable () where genAny = pure ()

instance Generable Bool where genAny = pure False <|> pure True

instance Generable Char where
  genAny = chr <$> (genBits 20 <|> (+ 0x100000) <$> genBits 16)

instance Generable Word8 where genAny = genBits 8

instance Generable Word16 where genAny = genBits 16

instance Generable Word32 where genAny = genBits 32

instance Generable Word64 where genAny = genBits 64

instance Generable Int8 where genAny = genBits 8

instance Generable Int16 where genAny = genBits 16

instance Generable Int32 where genAny = genBits 32

instance Generable Int64 where genAny = genBits 64

instance Generable Integer where
  genAny = pure 0 <|> genPositive <|> negate <$> genPositive

instance Generable a => Generable (Maybe a) where
  genAny = pure Nothing <|> Just <$> genAny

instance (Generable a, Generable b) => Generable (Either a b) where
  genAny = Left <$> genAny <|> Right <$> genAny

instance (Generable a, Generable b) => Generable (a, b) where
  genAny = (,) <$> genAny <*> genAny

instance Generable a => Generable [a] where
  genAny = pure [] <|> ((:) <$> genAny <*> genAny)
