{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

-- | TODO: Module documentation
module Trynocular
  ( -- * The 'Generator' type
    Generator,

    -- * The 'Key'
    Key,
    PartialKey,
    GeneralKey,
    KeyF (..),

    -- * Operations on 'Generator'
    pickKey,
    keys,
    fromKey,
    compatibleKey,
    toKey,
    pickValue,
    values,
    keySimilarity,
    valueSimilarity,

    -- * Demand and observation
    fromPartialKey,
    spy,

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
import Control.Concurrent (MVar, newMVar, readMVar, swapMVar)
import Data.Char (chr, ord)
import Data.Functor.Classes (Eq1 (..), Show1 (..), Ord1 (..))
import Data.Functor.Identity (Identity (..))
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
    go (Apply _ _ ga) = go ga

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
fromKey Trivial (Identity TrivialF) = ()
fromKey (Choice ga _) (Identity (LeftF k)) = Left (fromKey ga k)
fromKey (Choice _ gb) (Identity (RightF k)) = Right (fromKey gb k)
fromKey (Both ga gb) (Identity (BothF k1 k2)) =
  (fromKey ga k1, fromKey gb k2)
fromKey (Apply f _ ga) k = f (fromKey ga k)
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
fromPartialKey (Apply f _ ga) k = f (fromPartialKey ga k)
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
compatibleKey (Apply _ _ ga) k = compatibleKey ga k
compatibleKey _ _ = False

-- | Using a 'Generator', convert a value to a 'Key'.
--
-- The conversion preserves strictness, in the sense that it forces only enough
-- of the value to satisfy the demand on the resulting key.  In particular, one
-- should have @'fromKey' g . 'toKey' g == 'id'@ on any value produced by
-- the generator @g@, not just in equality but in strictness as well.
toKey :: Generator a -> a -> Key
toKey Trivial () = Identity TrivialF
toKey (Choice ga _) (Left a) = Identity (LeftF (toKey ga a))
toKey (Choice _ gb) (Right b) = Identity (RightF (toKey gb b))
toKey (Both ga gb) (a, b) = Identity (BothF (toKey ga a) (toKey gb b))
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

-- | Produces a similarity score between two values.  Similarity is between 0
-- and 1, and a key has a similarity of 1 only to itself.
keySimilarity :: Key -> Key -> Rational
keySimilarity (Identity TrivialF) (Identity TrivialF) = 1
keySimilarity (Identity (LeftF k1)) (Identity (LeftF k2)) =
  0.5 + 0.5 * keySimilarity k1 k2
keySimilarity (Identity (RightF k1)) (Identity (RightF k2)) =
  0.5 + 0.5 * keySimilarity k1 k2
keySimilarity (Identity (BothF k1 k2)) (Identity (BothF k3 k4)) =
  (keySimilarity k1 k3 + keySimilarity k2 k4) / 2
keySimilarity _ _ = 0

-- | Produces a similarity score between two values.  Similarity is between 0
-- and 1, and a value has a similarity of 1 only to itself.
valueSimilarity :: Generator a -> a -> a -> Rational
valueSimilarity g x y = keySimilarity (toKey g x) (toKey g y)

-- | A type representing the common structure of different kinds of 'GeneralKey'
-- types, which represent specific decisions in a generator.  The @f@ parameter
-- represents context that can be applied at each level.
data KeyF f
  = TrivialF
  | LeftF (GeneralKey f)
  | RightF (GeneralKey f)
  | BothF (GeneralKey f) (GeneralKey f)

instance Show1 f => Show (KeyF f) where
  showsPrec _ TrivialF = showString "TrivialF"
  showsPrec p (LeftF k) =
    showParen (p > 10) $
      showString "LeftF " . liftShowsPrec showsPrec showList 11 k
  showsPrec p (RightF k) =
    showParen (p > 10) $
      showString "RightF " . liftShowsPrec showsPrec showList 11 k
  showsPrec p (BothF k1 k2) =
    showParen (p > 10) $
      showString "BothF "
        . liftShowsPrec showsPrec showList 11 k1
        . showString " "
        . liftShowsPrec showsPrec showList 11 k2

instance Eq1 f => Eq (KeyF f) where
  TrivialF == TrivialF = True
  LeftF a == LeftF b = liftEq (==) a b
  RightF a == RightF b = liftEq (==) a b
  BothF a b == BothF c d = liftEq (==) a c && liftEq (==) b d
  _ == _ = False

instance Ord1 f => Ord (KeyF f) where
  compare TrivialF TrivialF = EQ
  compare TrivialF _ = LT
  compare (LeftF _) TrivialF = GT
  compare (LeftF a) (LeftF b) = liftCompare compare a b
  compare (LeftF _) _ = LT
  compare (RightF _) (BothF _ _) = LT
  compare (RightF a) (RightF b) = liftCompare compare a b
  compare (RightF _) _ = GT
  compare (BothF a b) (BothF c d) =
    case liftCompare compare a c of
      EQ -> liftCompare compare b d
      cmp -> cmp
  compare (BothF _ _) _ = GT

-- | A 'GeneralKey' is a key with some context attached to each node,
-- represented by a 'Functor' called @f@.
type GeneralKey f = f (KeyF f)

-- | A unique identifier for a total value produced by a 'Generator'.  You can
-- think of this as a "plan" that a 'Generator' can follow to produce a specific
-- value.
type Key = GeneralKey Identity

-- | A uniquely identifier for a partial value produced by a 'Generator'.
type PartialKey = GeneralKey Maybe

-- A 'GeneralKey' that contains all the information of a 'Key', and also keeps
-- track of whether any given node has been observed.
type ObservableKey = GeneralKey Observable

-- A 'Functor' used to implement 'ObservableKey'.
data Observable a = Observable (MVar Bool) a

-- A utility function that's useful for recursive conversion functions on
-- keys.
onSubkeys ::
  Monad m => (GeneralKey f1 -> m (GeneralKey f2)) -> KeyF f1 -> m (KeyF f2)
onSubkeys _ TrivialF = pure TrivialF
onSubkeys op (LeftF k) = LeftF <$> op k
onSubkeys op (RightF k) = RightF <$> op k
onSubkeys op (BothF k1 k2) = BothF <$> op k1 <*> op k2

-- Given a 'Key', produces an 'ObservableKey' that can keep track of demand.
makeObservable :: Key -> IO ObservableKey
makeObservable (Identity k) = do
  var <- newMVar False
  Observable var <$> onSubkeys makeObservable k

-- Given an 'ObservableKey', produces a 'PartialKey' that records how much of
-- the key has been demanded.
observe :: ObservableKey -> IO PartialKey
observe (Observable var k) = do
  forced <- readMVar var
  if forced then Just <$> onSubkeys observe k else pure Nothing

-- Given an 'ObservableKey', produces a 'Key' that behaves exactly like the
-- original (on which 'makeObservable' was called), but silently tracks demand
-- as it is consumed.
makeSpy :: ObservableKey -> IO Key
makeSpy (Observable var k) = unsafeInterleaveIO $ do
  _ <- swapMVar var True
  Identity <$> onSubkeys makeSpy k

-- | Runs an action on a 'Key', and returns the demand on the 'Key' (as a
-- 'PartialKey') in addition to the result of the action.
spy :: Key -> (Key -> IO a) -> IO (PartialKey, a)
spy k action = do
  ok <- makeObservable k
  x <- action =<< makeSpy ok
  (,x) <$> observe ok

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
            (\(b, x) -> b * (n `div` 2) + x)
            (\y -> if y >= n `div` 2 then (1, y - n `div` 2) else (0, y))
            (Both (go 2) (go (n `div` 2)))
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

-- | A 'Generator' that produces an arbitrary 'Float'.
genFloat :: Generator Float
genFloat
  | isIEEE (undefined :: Float)
      && floatRadix (undefined :: Float) == 2
      && floatDigits (undefined :: Float) == 24
      && floatRange (undefined :: Float) == (-125, 128) =
      transform unsafeCoerce unsafeCoerce (genBoundedIntegral @Word32)
  | otherwise = error "Float is not IEEE single-precision on this platform!"

-- | A 'Generator' that produces an arbitrary 'Double'.
genDouble :: Generator Double
genDouble
  | isIEEE (undefined :: Double)
      && floatRadix (undefined :: Double) == 2
      && floatDigits (undefined :: Double) == 53
      && floatRange (undefined :: Double) == (-1021, 1024) =
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
