{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module Trynocular where

import Control.Applicative (Alternative (..))
import Data.Char (chr, ord)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Kind (Type)
import Data.Universe.Helpers ((+*+), (+++))
import Data.Void (Void, absurd)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Generics
  ( Generic (..),
    K1 (..),
    M1 (..),
    U1 (..),
    V1,
    (:*:) (..),
    (:+:) (..),
  )
import System.Random (randomIO)

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
  pure x = Apply (\() -> x) Trivial
  a <*> b = Apply (uncurry ($)) (Both a b)

instance Alternative Generator where
  empty = Apply absurd Empty
  a <|> b = Apply (either id id) (Choice a b)

data GenKey
  = TrivialKey
  | LeftKey GenKey
  | RightKey GenKey
  | BothKey GenKey GenKey
  deriving (Eq, Ord, Show)

generateKey :: Generator a -> IO (Maybe GenKey)
generateKey Empty = return Nothing
generateKey Trivial = pure (Just TrivialKey)
generateKey (Choice ga gb) = do
  let left = fmap LeftKey <$> generateKey ga
      right = fmap RightKey <$> generateKey gb
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
  (LeftKey <$> enumerateKeys ga) +++ (RightKey <$> enumerateKeys gb)
enumerateKeys (Both ga gb) =
  uncurry BothKey <$> enumerateKeys ga +*+ enumerateKeys gb
enumerateKeys (Apply _ g) = enumerateKeys g

generateFromKey :: Generator a -> GenKey -> a
generateFromKey Trivial TrivialKey = ()
generateFromKey (Choice ga _) (LeftKey k) = Left (generateFromKey ga k)
generateFromKey (Choice _ gb) (RightKey k) = Right (generateFromKey gb k)
generateFromKey (Both ga gb) (BothKey k1 k2) =
  (generateFromKey ga k1, generateFromKey gb k2)
generateFromKey (Apply f ga) k = f (generateFromKey ga k)
generateFromKey _ _ = error "key doesn't match generator"

generateValue :: Generator a -> IO (Maybe a)
generateValue g = fmap (generateFromKey g) <$> generateKey g

enumerateValues :: Generator a -> [a]
enumerateValues g = generateFromKey g <$> enumerateKeys g

genRange :: Integral t => (Integer, Integer) -> Generator t
genRange (lo, hi) = fromInteger . (+ lo) <$> go (hi - lo + 1)
  where
    go 1 = pure 0
    go n
      | even n = (\x b -> 2 * x + b) <$> go (n `div` 2) <*> (pure 0 <|> pure 1)
      | otherwise = pure 0 <|> (succ <$> go (n - 1))

toGenKeyRange :: Integral t => (Integer, Integer) -> t -> GenKey
toGenKeyRange (lo, hi) = go (hi - lo + 1) . subtract lo . toInteger
  where
    go 1 _ = TrivialKey
    go n x
      | even n =
          BothKey
            (go (n `div` 2) (x `div` 2))
            (if even x then LeftKey TrivialKey else RightKey TrivialKey)
      | x == 0 = LeftKey TrivialKey
      | otherwise = RightKey (go (n - 1) (x - 1))

genBounded :: forall t. (Bounded t, Integral t) => Generator t
genBounded =
  genRange
    ( fromIntegral (minBound :: t),
      fromIntegral (maxBound :: t)
    )

toGenKeyBounded :: forall t. (Bounded t, Integral t) => t -> GenKey
toGenKeyBounded =
  toGenKeyRange
    ( fromIntegral (minBound :: t),
      fromIntegral (maxBound :: t)
    )

genPositive :: Generator Integer
genPositive =
  pure 1
    <|> (* 2) <$> genPositive
    <|> (\n -> 2 * n + 1) <$> genPositive

toGenKeyPositive :: Integer -> GenKey
toGenKeyPositive x
  | x == 1 = LeftKey (LeftKey TrivialKey)
  | even x = LeftKey (RightKey (toGenKeyPositive (x `div` 2)))
  | otherwise = RightKey (toGenKeyPositive ((x - 1) `div` 2))

class Generable a where
  genAny :: Generator a
  default genAny :: (Generic a, GenericGenerable (Rep a)) => Generator a
  genAny = genGeneric

  toGenKey :: a -> GenKey
  default toGenKey :: (Generic a, GenericGenerable (Rep a)) => a -> GenKey
  toGenKey = toGenKeyGeneric

instance Generable () where
  genAny = pure ()
  toGenKey () = TrivialKey

instance Generable Bool where
  genAny = pure False <|> pure True
  toGenKey False = LeftKey TrivialKey
  toGenKey True = RightKey TrivialKey

instance Generable Char where
  genAny = chr <$> genRange (0, 0x10FFFF)
  toGenKey = toGenKeyRange (0, 0x10FFFF) . ord

instance Generable Word8 where
  genAny = genBounded
  toGenKey = toGenKeyBounded

instance Generable Word16 where
  genAny = genBounded
  toGenKey = toGenKeyBounded

instance Generable Word32 where
  genAny = genBounded
  toGenKey = toGenKeyBounded

instance Generable Word64 where
  genAny = genBounded
  toGenKey = toGenKeyBounded

instance Generable Word where
  genAny = genBounded
  toGenKey = toGenKeyBounded

instance Generable Int8 where
  genAny = genBounded
  toGenKey = toGenKeyBounded

instance Generable Int16 where
  genAny = genBounded
  toGenKey = toGenKeyBounded

instance Generable Int32 where
  genAny = genBounded
  toGenKey = toGenKeyBounded

instance Generable Int64 where
  genAny = genBounded
  toGenKey = toGenKeyBounded

instance Generable Int where
  genAny = genBounded
  toGenKey = toGenKeyBounded

instance Generable Integer where
  genAny = pure 0 <|> genPositive <|> negate <$> genPositive
  toGenKey x
    | x == 0 = LeftKey (LeftKey TrivialKey)
    | x > 0 = LeftKey (RightKey (toGenKeyPositive x))
    | otherwise = RightKey (toGenKeyPositive (-x))

instance Generable a => Generable (Maybe a) where
  genAny = pure Nothing <|> Just <$> genAny
  toGenKey Nothing = LeftKey TrivialKey
  toGenKey (Just x) = RightKey (toGenKey x)

instance (Generable a, Generable b) => Generable (Either a b) where
  genAny = Left <$> genAny <|> Right <$> genAny
  toGenKey (Left x) = LeftKey (toGenKey x)
  toGenKey (Right x) = RightKey (toGenKey x)

instance (Generable a, Generable b) => Generable (a, b) where
  genAny = (,) <$> genAny <*> genAny
  toGenKey (a, b) = BothKey (toGenKey a) (toGenKey b)

instance (Generable a, Generable b, Generable c) => Generable (a, b, c) where
  genAny = (,,) <$> genAny <*> genAny <*> genAny
  toGenKey (a, b, c) = BothKey (BothKey (toGenKey a) (toGenKey b)) (toGenKey c)

instance
  (Generable a, Generable b, Generable c, Generable d) =>
  Generable (a, b, c, d)
  where
  genAny = (,,,) <$> genAny <*> genAny <*> genAny <*> genAny
  toGenKey (a, b, c, d) =
    BothKey
      (BothKey (BothKey (toGenKey a) (toGenKey b)) (toGenKey c))
      (toGenKey d)

instance
  (Generable a, Generable b, Generable c, Generable d, Generable e) =>
  Generable (a, b, c, d, e)
  where
  genAny = (,,,,) <$> genAny <*> genAny <*> genAny <*> genAny <*> genAny
  toGenKey (a, b, c, d, e) =
    BothKey
      ( BothKey
          (BothKey (BothKey (toGenKey a) (toGenKey b)) (toGenKey c))
          (toGenKey d)
      )
      (toGenKey e)

instance Generable a => Generable [a] where
  genAny = pure [] <|> ((:) <$> genAny <*> genAny)
  toGenKey [] = LeftKey TrivialKey
  toGenKey (x : xs) = RightKey (BothKey (toGenKey x) (toGenKey xs))

class GenericGenerable f where
  genGenericRep :: Generator (f p)
  toGenKeyGenericRep :: f p -> GenKey

instance GenericGenerable V1 where
  genGenericRep = empty
  toGenKeyGenericRep v = case v of {}

instance GenericGenerable U1 where
  genGenericRep = pure U1
  toGenKeyGenericRep _ = TrivialKey

instance
  (GenericGenerable f, GenericGenerable g) =>
  GenericGenerable (f :+: g)
  where
  genGenericRep = L1 <$> genGenericRep <|> R1 <$> genGenericRep
  toGenKeyGenericRep (L1 x) = LeftKey (toGenKeyGenericRep x)
  toGenKeyGenericRep (R1 y) = RightKey (toGenKeyGenericRep y)

instance
  (GenericGenerable f, GenericGenerable g) =>
  GenericGenerable (f :*: g)
  where
  genGenericRep = (:*:) <$> genGenericRep <*> genGenericRep
  toGenKeyGenericRep (x :*: y) =
    BothKey (toGenKeyGenericRep x) (toGenKeyGenericRep y)

instance Generable a => GenericGenerable (K1 i a) where
  genGenericRep = K1 <$> genAny
  toGenKeyGenericRep (K1 x) = toGenKey x

instance GenericGenerable f => GenericGenerable (M1 i t f) where
  genGenericRep = M1 <$> genGenericRep
  toGenKeyGenericRep (M1 x) = toGenKeyGenericRep x

genGeneric :: (Generic a, GenericGenerable (Rep a)) => Generator a
genGeneric = to <$> genGenericRep

toGenKeyGeneric :: (Generic a, GenericGenerable (Rep a)) => a -> GenKey
toGenKeyGeneric = toGenKeyGenericRep . from
