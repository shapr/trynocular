{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

-- | TODO: Module documentation
module Trynocular
  ( -- * The 'Generator' and 'GenKey' types
    Generator,
    GenKey (..),

    -- * Operations on 'Generator'
    pickGenKey,
    genKeys,
    fromGenKey,
    toGenKey,
    pickValue,
    values,

    -- * Manual construction of 'Generator's
    genOnly,
    genMaybe,
    genEither,
    genPair,
    genOneOf,
    genCases,
    genRange,
    genBoundedIntegral,
    genInteger,
    genPositive,
    genNonNegative,
    invmap,

    -- * 'Generable' type class
    Generable (..),
  )
where

import Data.Char (chr, ord)
import Data.Functor.Invariant (Invariant (..))
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Kind (Type)
import Data.Universe.Helpers ((+*+), (+++))
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Generics
  ( Generic (..),
    K1 (..),
    M1 (..),
    U1 (..),
    (:*:) (..),
    (:+:) (..),
  )
import System.Random (randomIO)
import System.IO.Unsafe (unsafeInterleaveIO)

-- | A 'Generator' for a type knows how to enumerate or pick values for the
-- type, as well as how to represent values of the type as 'GenKey's.  The
-- representation as 'GenKey's is witnessed by 'toGenKey' and 'fromGenKey'.
-- This correspondence preserves not only equality, but strictness as well.
data Generator :: Type -> Type where
  Trivial :: Generator ()
  Choice :: Generator a -> Generator b -> Generator (Either a b)
  Both :: Generator a -> Generator b -> Generator (a, b)
  Apply :: (a -> b) -> (b -> a) -> Generator a -> Generator b

instance Invariant Generator where
  invmap = Apply

-- | A key (intuitively, a record of paths taken in a decision tree) that
-- uniquely identifies a value produced by a Generator.
data GenKey
  = TrivialKey
  | LeftKey GenKey
  | RightKey GenKey
  | BothKey GenKey GenKey
  deriving (Eq, Ord, Show)

-- | Choose a single 'GenKey' corresponding to a value from a 'Generator'.
--
-- This uses lazy IO to generate the random path, so it is cheap if you don't
-- force too much of the resulting key.
pickGenKey :: Generator a -> IO GenKey
pickGenKey = unsafeInterleaveIO . go
  where
    go :: Generator a -> IO GenKey
    go Trivial = pure TrivialKey
    go (Choice ga gb) =
      randomIO >>= \case
        True -> LeftKey <$> pickGenKey ga
        False -> RightKey <$> pickGenKey gb
    go (Both ga gb) = BothKey <$> pickGenKey ga <*> pickGenKey gb
    go (Apply _ _ ga) = go ga

-- | List all 'GenKey's corresponding to values from a 'Generator'.
--
-- TODO: This currently fails on generators that contain infinite paths in the
-- left branch of a choice. See issue #1.
genKeys :: Generator a -> [GenKey]
genKeys Trivial = [TrivialKey]
genKeys (Choice ga gb) =
  (LeftKey <$> genKeys ga) +++ (RightKey <$> genKeys gb)
genKeys (Both ga gb) =
  uncurry BothKey <$> genKeys ga +*+ genKeys gb
genKeys (Apply _ _ g) = genKeys g

-- | Using a 'Generator', convert a 'GenKey' to a value.
--
-- This is a partial function.  If the provided 'GenKey' is not compatible with
-- the given generator, it will fail to return a total value.
--
-- The conversion preserves strictness, in the sense that it forces only enough
-- of the key to satisfy the demand on the resulting value.  In particular, one
-- should have @'fromGenKey' g . 'toGenKey' g == 'id'@ on any value produced by
-- the generator @g@, not just in equality but in strictness as well.
fromGenKey :: Generator a -> GenKey -> a
fromGenKey Trivial TrivialKey = ()
fromGenKey (Choice ga _) (LeftKey k) = Left (fromGenKey ga k)
fromGenKey (Choice _ gb) (RightKey k) = Right (fromGenKey gb k)
fromGenKey (Both ga gb) (BothKey k1 k2) =
  (fromGenKey ga k1, fromGenKey gb k2)
fromGenKey (Apply f _ ga) k = f (fromGenKey ga k)
fromGenKey _ _ = error "key doesn't match generator"

-- | Using a 'Generator', convert a value to a 'GenKey'.
--
-- The conversion preserves strictness, in the sense that it forces only enough
-- of the value to satisfy the demand on the resulting key.  In particular, one
-- should have @'fromGenKey' g . 'toGenKey' g == 'id'@ on any value produced by
-- the generator @g@, not just in equality but in strictness as well.
toGenKey :: Generator a -> a -> GenKey
toGenKey Trivial () = TrivialKey
toGenKey (Choice ga _) (Left a) = LeftKey (toGenKey ga a)
toGenKey (Choice _ gb) (Right b) = RightKey (toGenKey gb b)
toGenKey (Both ga gb) (a, b) = BothKey (toGenKey ga a) (toGenKey gb b)
toGenKey (Apply _ f g) x = toGenKey g (f x)

-- | Pick a value from a 'Generator'.
--
-- This uses lazy IO to make random choices, so it is cheap if you don't force
-- too much of the resulting value.
pickValue :: Generator a -> IO a
pickValue g = fromGenKey g <$> pickGenKey g

-- | List all values for a 'Generator'.
--
-- TODO: This currently fails on generators that contain infinite paths in the
-- left branch of a choice. See issue #1.
values :: Generator a -> [a]
values g = fromGenKey g <$> genKeys g

-- | A 'Generator' that always generates the same value.
--
-- In terms of strictness, the argument is typically a "flat" value, whose only
-- approximants are bottom and the value itself.  Otherwise, any demand at all
-- on the value will generate the same demand on the key, so the key won't
-- represent the full lattice of possible demands.
genOnly :: a -> Generator a
genOnly x = invmap (\() -> x) (\y -> y `seq` ()) Trivial

-- | A 'Generator' that produces an 'Either' value, with the given generators
-- for the left and right sides.
genEither :: Generator a -> Generator b -> Generator (Either a b)
genEither = Choice

-- | A 'Generator' that produces a pair of values, with the given generators for
-- the left and right sides.
genPair :: Generator a -> Generator b -> Generator (a, b)
genPair = Both

-- | A 'Generator' that produces a 'Maybe' value, with the given generator for
-- the value if present.
genMaybe :: Generator a -> Generator (Maybe a)
genMaybe g =
  invmap
    (either (const Nothing) Just)
    (maybe (Left ()) Right)
    (Choice Trivial g)

-- | A 'Generator' that produces a value by case analysis.  The first argument
-- is a list of cases, each with a predicate that matches values produced by the
-- case's 'Generator'.  The second argument is a fallthrough for values that
-- match none of the predicates.  You can think of this as like guards with the
-- fallthrough as the 'otherwise' branch, except that the predicates match the
-- result instead of an input.
--
-- A 'Generator' in the list should never produce a value that matches a
-- predicate for an earlier case.  The fallthrough 'Generator' should never
-- produce a value that matches the predicate for any case.
genCases :: [(a -> Bool, Generator a)] -> Generator a -> Generator a
genCases [] g = g
genCases ((p, g) : gs) fallthrough =
  invmap
    (either id id)
    (\x -> if p x then Left x else Right x)
    (Choice g (genCases gs fallthrough))

-- | A 'Generator' that produces a value from the given list.  The list should
-- be non-empty, and should not contain duplicates.
genOneOf :: Eq a => [a] -> Generator a
genOneOf [] = error "empty list of choices"
genOneOf xs = genCases [((== x), genOnly x) | x <- init xs] (genOnly (last xs))

-- | A 'Generator' that produces a value of an 'Integral' type from the given
-- inclusive range.
genRange :: Integral t => (Integer, Integer) -> Generator t
genRange (lo, hi) =
  invmap fromInteger toInteger
    . invmap (+ lo) (subtract lo)
    $ go (hi - lo + 1)
  where
    go n
      | n == 1 = genOnly 0
      | n == 2 = genOneOf [0, 1]
      | even n =
          invmap
            (\(x, b) -> 2 * x + b)
            (`divMod` 2)
            (Both (go (n `div` 2)) (go 2))
      | otherwise =
          genCases
            [((== 0), genOnly 0)]
            (invmap (+ 1) (subtract 1) (go (n - 1)))

-- | A 'Generator' that produces a value of an 'Integral' type from its full
-- range.
genBoundedIntegral :: forall t. (Bounded t, Integral t) => Generator t
genBoundedIntegral =
  genRange
    ( fromIntegral (minBound :: t),
      fromIntegral (maxBound :: t)
    )

-- | A 'Generator' that produces a positive 'Integer'.
genPositive :: Generator Integer
genPositive =
  genCases
    [((== 1), genOnly 1), (even, invmap (* 2) (`div` 2) genPositive)]
    (invmap (succ . (* 2)) (`div` 2) genPositive)

-- | A 'Generator' that produces a non-negative 'Integer'.
genNonNegative :: Generator Integer
genNonNegative = genCases [((== 0), genOnly 0)] genPositive

-- | A 'Generator' that produces an arbitrary 'Integer'.
genInteger :: Generator Integer
genInteger =
  genCases
    [((== 0), genOnly 0), ((> 0), genPositive)]
    (invmap negate negate genPositive)

-- | A type class for types that can be generated by a 'Generator'.  For types
-- that are an instance of this class, 'genAny' can be used as a generator for
-- an arbitrary value of that type.
class Generable a where
  genAny :: Generator a
  default genAny :: (Generic a, GenericGenerable (Rep a)) => Generator a
  genAny = genGeneric

instance Generable ()

instance Generable Bool

instance Generable Char where
  genAny = invmap chr ord (genRange (0, 0x10FFFF))

instance Generable Word8 where
  genAny = genBoundedIntegral

instance Generable Word16 where
  genAny = genBoundedIntegral

instance Generable Word32 where
  genAny = genBoundedIntegral

instance Generable Word64 where
  genAny = genBoundedIntegral

instance Generable Word where
  genAny = genBoundedIntegral

instance Generable Int8 where
  genAny = genBoundedIntegral

instance Generable Int16 where
  genAny = genBoundedIntegral

instance Generable Int32 where
  genAny = genBoundedIntegral

instance Generable Int64 where
  genAny = genBoundedIntegral

instance Generable Int where
  genAny = genBoundedIntegral

instance Generable Integer where
  genAny = genInteger

instance Generable a => Generable (Maybe a)

instance (Generable a, Generable b) => Generable (Either a b)

instance (Generable a, Generable b) => Generable (a, b)

instance (Generable a, Generable b, Generable c) => Generable (a, b, c)

instance
  (Generable a, Generable b, Generable c, Generable d) =>
  Generable (a, b, c, d)

instance
  (Generable a, Generable b, Generable c, Generable d, Generable e) =>
  Generable (a, b, c, d, e)

instance Generable a => Generable [a]

class GenericGenerable f where
  genGenericRep :: Generator (f p)

instance GenericGenerable U1 where
  genGenericRep = genOnly U1

instance
  (GenericGenerable f, GenericGenerable g) =>
  GenericGenerable (f :+: g)
  where
  genGenericRep =
    invmap
      (either L1 R1)
      (\y -> case y of L1 l -> Left l; R1 r -> Right r)
      (Choice genGenericRep genGenericRep)

instance
  (GenericGenerable f, GenericGenerable g) =>
  GenericGenerable (f :*: g)
  where
  genGenericRep =
    invmap
      (uncurry (:*:))
      (\(x :*: y) -> (x, y))
      (Both genGenericRep genGenericRep)

instance Generable a => GenericGenerable (K1 i a) where
  genGenericRep = invmap K1 (\(K1 x) -> x) genAny

instance GenericGenerable f => GenericGenerable (M1 i t f) where
  genGenericRep = invmap M1 (\(M1 x) -> x) genGenericRep

genGeneric :: (Generic a, GenericGenerable (Rep a)) => Generator a
genGeneric = invmap to from genGenericRep
