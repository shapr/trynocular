{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

-- | A 'Generator' knows how to generate 'Key's and convert them into values for
-- a type.  This module contains the 'Generator' type, some operations on
-- 'Generator's, and some helpers for building 'Generator's.
module Trynocular.Generator
  ( -- * The 'Generator' type
    Generator,

    -- * Operations on 'Generator'
    pickKey,
    keys,
    fromKey,
    fromPartialKey,
    compatibleKey,
    pickValue,
    values,

    -- * Manual construction of 'Generator's
    genRange,
    genBoundedIntegral,
    genPositive,
    genNonNegative,
  )
where

import Data.Functor.Alt (Alt (..))
import Data.Functor.Identity (Identity (..))
import Data.Kind (Type)
import Data.Universe.Helpers ((+*+), (+++))
import System.IO.Unsafe (unsafeInterleaveIO)
import System.Random (randomIO)
import Trynocular.Key (Key, KeyF (..), PartialKey)

-- | A 'Generator' for a type knows how to enumerate or pick values for the
-- type in terms of their 'Key', and how to actually build those values using
-- 'fromKey'.  Crucially, 'fromKey' is maximally lazy in the 'Key'.
data Generator :: Type -> Type where
  Trivial :: Generator ()
  Choice :: Generator a -> Generator b -> Generator (Either a b)
  Both :: Generator a -> Generator b -> Generator (a, b)
  Apply :: (a -> b) -> Generator a -> Generator b

instance Functor Generator where
  fmap = Apply

instance Alt Generator where
  a <!> b = either id id <$> Choice a b

instance Applicative Generator where
  pure x =
    Apply
      (\() -> x) -- Strictness is deliberate here, so that demand of the
      -- value is observable in demand of the key.
      Trivial
  f <*> x = uncurry ($) <$> Both f x

-- | Choose a single 'Key' corresponding to a value from a 'Generator'.
--
-- This uses lazy IO to generate the random path, so it is cheap if you don't
-- force too much of the resulting key.
pickKey :: Generator a -> IO Key
pickKey = unsafeInterleaveIO . go
  where
    go :: Generator a -> IO Key
    go Trivial = pure (Identity TrivialF)
    go (Choice ga gb) =
      randomIO >>= \case
        True -> Identity . LeftF <$> pickKey ga
        False -> Identity . RightF <$> pickKey gb
    go (Both ga gb) = Identity <$> (BothF <$> pickKey ga <*> pickKey gb)
    go (Apply _ ga) = go ga

-- | List all 'Key's corresponding to values from a 'Generator'.
--
-- TODO: This currently fails on generators that contain infinite paths in the
-- left branch of a choice. See issue #1.
keys :: Generator a -> [Key]
keys Trivial = [Identity TrivialF]
keys (Choice ga gb) =
  (Identity . LeftF <$> keys ga) +++ (Identity . RightF <$> keys gb)
keys (Both ga gb) =
  Identity <$> (uncurry BothF <$> keys ga +*+ keys gb)
keys (Apply _ g) = keys g

-- | Using a 'Generator', convert a 'Key' to a value.
--
-- This is a partial function.  If the provided 'Key' is not compatible with
-- the given generator, it will fail to return a total value.
--
-- The conversion is maximally lazy in the 'Key', in the sense that it forces
-- only as much of the key as needed to satisfy the demand on the resulting
-- value.
fromKey :: Generator a -> Key -> a
fromKey Trivial (Identity TrivialF) = ()
fromKey (Choice ga _) (Identity (LeftF k)) = Left (fromKey ga k)
fromKey (Choice _ gb) (Identity (RightF k)) = Right (fromKey gb k)
fromKey (Both ga gb) (Identity (BothF k1 k2)) =
  (fromKey ga k1, fromKey gb k2)
fromKey (Apply f ga) k = f (fromKey ga k)
fromKey _ _ = error "key doesn't match generator"

-- | Using a 'Generator', convert a 'PartialKey' to a value.
--
-- This is just like 'fromKey', except that it can produce values containing
-- 'undefined' when the key isn't specified.
fromPartialKey :: Generator a -> PartialKey -> a
fromPartialKey _ Nothing = undefined
fromPartialKey Trivial (Just TrivialF) = ()
fromPartialKey (Choice ga _) (Just (LeftF k)) = Left (fromPartialKey ga k)
fromPartialKey (Choice _ gb) (Just (RightF k)) = Right (fromPartialKey gb k)
fromPartialKey (Both ga gb) (Just (BothF k1 k2)) =
  (fromPartialKey ga k1, fromPartialKey gb k2)
fromPartialKey (Apply f ga) k = f (fromPartialKey ga k)
fromPartialKey _ _ = error "key doesn't match generator"

-- | Checks whether a given 'Key' is compatible with this 'Generator'.  If the
-- result is 'True', then 'fromKey' will succeed and produce a total value.
-- Otherwise, it will produce a value containing bottoms.
compatibleKey :: Generator a -> Key -> Bool
compatibleKey Trivial (Identity TrivialF) = True
compatibleKey (Choice ga _) (Identity (LeftF k)) = compatibleKey ga k
compatibleKey (Choice _ gb) (Identity (RightF k)) = compatibleKey gb k
compatibleKey (Both ga gb) (Identity (BothF k1 k2)) =
  compatibleKey ga k1 && compatibleKey gb k2
compatibleKey (Apply _ ga) k = compatibleKey ga k
compatibleKey _ _ = False

-- | Pick a value from a 'Generator'.
--
-- This uses lazy IO to make random choices, so it is cheap if you don't force
-- too much of the resulting value.
pickValue :: Generator a -> IO a
pickValue g = fromKey g <$> pickKey g

-- | List all values for a 'Generator'.
--
-- TODO: This currently fails on generators that contain infinite paths in the
-- left branch of a choice. See issue #1.
values :: Generator a -> [a]
values g = fromKey g <$> keys g

-- | A 'Generator' that produces a value of an 'Integral' type from the given
-- inclusive range.
genRange :: Integral t => (t, t) -> Generator t
genRange (toInteger -> lo, toInteger -> hi) =
  fmap (fromInteger . (+ lo)) $ go (hi - lo + 1)
  where
    go n
      | n == 1 = pure 0
      | n == 2 = pure 0 <!> pure 1
      | even n = (\b x -> b * (n `div` 2) + x) <$> go 2 <*> go (n `div` 2)
      | otherwise = pure 0 <!> succ <$> go (n - 1)

-- | A 'Generator' that produces a value of an 'Integral' type from its full
-- range.
genBoundedIntegral :: forall t. (Bounded t, Integral t) => Generator t
genBoundedIntegral = genRange (minBound, maxBound)

-- | A 'Generator' that produces a positive 'Integer'.
genPositive :: Generator Integer
genPositive = (\x b -> 2 * x + b) <$> genNonNegative <*> (pure 1 <!> pure 2)

-- | A 'Generator' that produces a non-negative 'Integer'.
genNonNegative :: Generator Integer
genNonNegative = pure 0 <!> genPositive