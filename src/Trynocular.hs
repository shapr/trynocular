{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

-- | TODO: Module documentation
module Trynocular
  ( -- * The 'Generator' and 'Key' types
    Generator,
    Key (..),

    -- * Operations on 'Generator'
    pickKey,
    keys,
    fromKey,
    toKey,
    pickValue,
    values,

    -- * Manual construction of 'Generator's
    genOnly,
    genMaybe,
    genEither,
    genPair,
    gen3Tuple,
    gen4Tuple,
    gen5Tuple,
    genList,
    genOneOf,
    genCases,
    genRange,
    genBoundedIntegral,
    genInteger,
    genPositive,
    genNonNegative,
    genFloat,
    genDouble,
    transform,

    -- * 'Generable' type class
    Generable (..),
    GenericGenerable (..),
  )
where

import Control.Arrow ((&&&))
import Data.Char (chr, ord)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Kind (Type)
import Data.Maybe (fromJust, isNothing)
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
import System.IO.Unsafe (unsafeInterleaveIO)
import System.Random (randomIO)
import Unsafe.Coerce (unsafeCoerce)

-- | A 'Generator' for a type knows how to enumerate or pick values for the
-- type, as well as how to represent values of the type as 'Key's.  The
-- representation as 'Key's is witnessed by 'toKey' and 'fromKey'.
-- This correspondence preserves not only equality, but strictness as well.
data Generator :: Type -> Type where
  Trivial :: Generator ()
  Choice :: Generator a -> Generator b -> Generator (Either a b)
  Both :: Generator a -> Generator b -> Generator (a, b)
  Apply :: (a -> b) -> (b -> a) -> Generator a -> Generator b

-- | Converts a 'Generator' from one type to another, by applying conversion
-- functions to the values.
--
-- A valid use of @'transform' f1 f2 gen@ requires that, for any value @x@
-- produced by @gen@:
--
-- 1. @f2 (f1 x) = x@.  This equality need not preserve strictness.
-- 2. @(f1 . f2) (f1 x) = f1 x@, and @f1 . f2@ *must* preserve strictness in
--    this case.
--
-- The conversion functions are automatically made strict on flat domains, so
-- one need only worry about strictness on nested domains.
transform :: (a -> b) -> (b -> a) -> Generator a -> Generator b
transform f1 f2 = Apply (\x -> x `seq` f1 x) (\y -> y `seq` f2 y)

-- | A 'Key' (intuitively, a record of paths taken in a decision tree) that
-- uniquely identifies a value produced by a Generator.
data Key
  = TrivialKey
  | LeftKey Key
  | RightKey Key
  | BothKey Key Key
  deriving (Eq, Ord, Show)

-- | Choose a single 'Key' corresponding to a value from a 'Generator'.
--
-- This uses lazy IO to generate the random path, so it is cheap if you don't
-- force too much of the resulting key.
pickKey :: Generator a -> IO Key
pickKey = unsafeInterleaveIO . go
  where
    go :: Generator a -> IO Key
    go Trivial = pure TrivialKey
    go (Choice ga gb) =
      randomIO >>= \case
        True -> LeftKey <$> pickKey ga
        False -> RightKey <$> pickKey gb
    go (Both ga gb) = BothKey <$> pickKey ga <*> pickKey gb
    go (Apply _ _ ga) = go ga

-- | List all 'Key's corresponding to values from a 'Generator'.
--
-- TODO: This currently fails on generators that contain infinite paths in the
-- left branch of a choice. See issue #1.
keys :: Generator a -> [Key]
keys Trivial = [TrivialKey]
keys (Choice ga gb) =
  (LeftKey <$> keys ga) +++ (RightKey <$> keys gb)
keys (Both ga gb) =
  uncurry BothKey <$> keys ga +*+ keys gb
keys (Apply _ _ g) = keys g

-- | Using a 'Generator', convert a 'Key' to a value.
--
-- This is a partial function.  If the provided 'Key' is not compatible with
-- the given generator, it will fail to return a total value.
--
-- The conversion preserves strictness, in the sense that it forces only enough
-- of the key to satisfy the demand on the resulting value.  In particular, one
-- should have @'fromKey' g . 'toKey' g == 'id'@ on any value produced by
-- the generator @g@, not just in equality but in strictness as well.
fromKey :: Generator a -> Key -> a
fromKey Trivial TrivialKey = ()
fromKey (Choice ga _) (LeftKey k) = Left (fromKey ga k)
fromKey (Choice _ gb) (RightKey k) = Right (fromKey gb k)
fromKey (Both ga gb) (BothKey k1 k2) =
  (fromKey ga k1, fromKey gb k2)
fromKey (Apply f _ ga) k = f (fromKey ga k)
fromKey _ _ = error "key doesn't match generator"

-- | Using a 'Generator', convert a value to a 'Key'.
--
-- The conversion preserves strictness, in the sense that it forces only enough
-- of the value to satisfy the demand on the resulting key.  In particular, one
-- should have @'fromKey' g . 'toKey' g == 'id'@ on any value produced by
-- the generator @g@, not just in equality but in strictness as well.
toKey :: Generator a -> a -> Key
toKey Trivial () = TrivialKey
toKey (Choice ga _) (Left a) = LeftKey (toKey ga a)
toKey (Choice _ gb) (Right b) = RightKey (toKey gb b)
toKey (Both ga gb) (a, b) = BothKey (toKey ga a) (toKey gb b)
toKey (Apply _ f g) x = toKey g (f x)

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

-- | A 'Generator' that always generates the same value.
--
-- In terms of strictness, the argument is typically a "flat" value, whose only
-- approximants are bottom and the value itself.  Otherwise, any demand at all
-- on the value will generate the same demand on the key, so the key won't
-- represent the full lattice of possible demands.
genOnly :: a -> Generator a
genOnly x = transform (const x) (const ()) Trivial

-- | A 'Generator' that produces an 'Either' value, with the given generators
-- for the left and right sides.
genEither :: Generator a -> Generator b -> Generator (Either a b)
genEither = Choice

-- | A 'Generator' that produces a pair of values, with the given generators for
-- the left and right sides.
genPair :: Generator a -> Generator b -> Generator (a, b)
genPair = Both

-- | A 'Generator' that produces a 3-tuple of values, with the given generators
-- for the components.
gen3Tuple :: Generator a -> Generator b -> Generator c -> Generator (a, b, c)
gen3Tuple ga gb gc =
  transform
    (\((a, b), c) -> (a, b, c))
    (\(a, b, c) -> ((a, b), c))
    (genPair (genPair ga gb) gc)

-- | A 'Generator' that produces a 4-tuple of values, with the given generators
-- for the components.
gen4Tuple ::
  Generator a ->
  Generator b ->
  Generator c ->
  Generator d ->
  Generator (a, b, c, d)
gen4Tuple ga gb gc gd =
  transform
    (\(((a, b), c), d) -> (a, b, c, d))
    (\(a, b, c, d) -> (((a, b), c), d))
    (genPair (genPair (genPair ga gb) gc) gd)

-- | A 'Generator' that produces a 5-tuple of values, with the given generators
-- for the components.
gen5Tuple ::
  Generator a ->
  Generator b ->
  Generator c ->
  Generator d ->
  Generator e ->
  Generator (a, b, c, d, e)
gen5Tuple ga gb gc gd ge =
  transform
    (\(((((a, b), c), d), e)) -> (a, b, c, d, e))
    (\(a, b, c, d, e) -> (((((a, b), c), d), e)))
    (genPair (genPair (genPair (genPair ga gb) gc) gd) ge)

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
genCases [] fallthrough = fallthrough
genCases ((p, g) : gs) fallthrough =
  transform
    (either id id)
    (\x -> if p x then Left x else Right x)
    (genEither g (genCases gs fallthrough))

-- | A 'Generator' that produces a 'Maybe' value, with the given generator for
-- the value if present.
genMaybe :: Generator a -> Generator (Maybe a)
genMaybe g = genCases [(isNothing, genOnly Nothing)] (transform Just fromJust g)

-- | A 'Generator' for lists, whose elements are produced by the given
-- generator.
genList :: Generator a -> Generator [a]
genList g =
  genCases
    [(null, genOnly [])]
    (transform (uncurry (:)) (head &&& tail) (genPair g (genList g)))

-- | A 'Generator' that produces a value from the given list.  The list should
-- be non-empty, and should not contain duplicates.
genOneOf :: Eq a => [a] -> Generator a
genOneOf [] = error "empty list of choices"
genOneOf xs = genCases [((== x), genOnly x) | x <- init xs] (genOnly (last xs))

-- | A 'Generator' that produces a value of an 'Integral' type from the given
-- inclusive range.
genRange :: Integral t => (t, t) -> Generator t
genRange (lo', hi') =
  transform fromInteger toInteger
    . transform (+ lo) (subtract lo)
    $ go (hi - lo + 1)
  where
    lo = toInteger lo'
    hi = toInteger hi'
    go n
      | n == 1 = genOnly 0
      | n == 2 = genOneOf [0, 1]
      | even n =
          transform
            (\(x, b) -> 2 * x + b)
            (`divMod` 2)
            (Both (go (n `div` 2)) (go 2))
      | otherwise =
          genCases
            [((== 0), genOnly 0)]
            (transform (+ 1) (subtract 1) (go (n - 1)))

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
    [((== 1), genOnly 1), (even, transform (* 2) (`div` 2) genPositive)]
    (transform (succ . (* 2)) (`div` 2) genPositive)

-- | A 'Generator' that produces a non-negative 'Integer'.
genNonNegative :: Generator Integer
genNonNegative = genCases [((== 0), genOnly 0)] genPositive

-- | A 'Generator' that produces an arbitrary 'Integer'.
genInteger :: Generator Integer
genInteger =
  genCases
    [((== 0), genOnly 0), ((> 0), genPositive)]
    (transform negate negate genPositive)

genFloat :: Generator Float
genFloat
  | isIEEE (undefined :: Float) &&
    floatRadix (undefined :: Float) == 2 &&
    floatDigits (undefined :: Float) == 24 &&
    floatRange (undefined :: Float) == (-125, 128) =
    transform unsafeCoerce unsafeCoerce (genBoundedIntegral @Word32)
  | otherwise = error "Float is not IEEE single-precision on this platform!"

genDouble :: Generator Double
genDouble
  | isIEEE (undefined :: Double) &&
    floatRadix (undefined :: Double) == 2 &&
    floatDigits (undefined :: Double) == 53 &&
    floatRange (undefined :: Double) == (-1021, 1024) =
    transform unsafeCoerce unsafeCoerce (genBoundedIntegral @Word64)
  | otherwise = error "Double is not IEEE double-precision on this platform!"

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
  genAny = transform chr ord (genRange (0, 0x10FFFF))

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

instance Generable Float where genAny = genFloat

instance Generable Double where genAny = genDouble

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

-- | A type class for GHC Generics constructors that can be used to generate a
-- Generable instance for a type.
class GenericGenerable f where
  genGenericRep :: Generator (f p)

instance GenericGenerable U1 where
  genGenericRep = genOnly U1

instance
  (GenericGenerable f, GenericGenerable g) =>
  GenericGenerable (f :+: g)
  where
  genGenericRep =
    transform
      (either L1 R1)
      (\y -> case y of L1 l -> Left l; R1 r -> Right r)
      (Choice genGenericRep genGenericRep)

instance
  (GenericGenerable f, GenericGenerable g) =>
  GenericGenerable (f :*: g)
  where
  genGenericRep =
    transform
      (uncurry (:*:))
      (\(x :*: y) -> (x, y))
      (Both genGenericRep genGenericRep)

instance Generable a => GenericGenerable (K1 i a) where
  genGenericRep = transform K1 (\(K1 x) -> x) genAny

instance GenericGenerable f => GenericGenerable (M1 i t f) where
  genGenericRep = transform M1 (\(M1 x) -> x) genGenericRep

genGeneric :: (Generic a, GenericGenerable (Rep a)) => Generator a
genGeneric = transform to from genGenericRep
