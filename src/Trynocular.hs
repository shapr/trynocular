{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module Trynocular
  ( -- * The 'Generator' type
    Generator,

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

data Generator :: Type -> Type where
  Trivial :: Generator ()
  Choice :: Generator a -> Generator b -> Generator (Either a b)
  Both :: Generator a -> Generator b -> Generator (a, b)
  Apply :: (a -> b) -> (b -> a) -> Generator a -> Generator b

instance Invariant Generator where
  invmap = Apply

data GenKey
  = TrivialKey
  | LeftKey GenKey
  | RightKey GenKey
  | BothKey GenKey GenKey
  deriving (Eq, Ord, Show)

pickGenKey :: Generator a -> IO GenKey
pickGenKey Trivial = pure TrivialKey
pickGenKey (Choice ga gb) =
  randomIO >>= \case
    True -> LeftKey <$> pickGenKey ga
    False -> RightKey <$> pickGenKey gb
pickGenKey (Both ga gb) = BothKey <$> pickGenKey ga <*> pickGenKey gb
pickGenKey (Apply _ _ ga) = pickGenKey ga

genKeys :: Generator a -> [GenKey]
genKeys Trivial = [TrivialKey]
genKeys (Choice ga gb) =
  (LeftKey <$> genKeys ga) +++ (RightKey <$> genKeys gb)
genKeys (Both ga gb) =
  uncurry BothKey <$> genKeys ga +*+ genKeys gb
genKeys (Apply _ _ g) = genKeys g

fromGenKey :: Generator a -> GenKey -> a
fromGenKey Trivial TrivialKey = ()
fromGenKey (Choice ga _) (LeftKey k) = Left (fromGenKey ga k)
fromGenKey (Choice _ gb) (RightKey k) = Right (fromGenKey gb k)
fromGenKey (Both ga gb) (BothKey k1 k2) =
  (fromGenKey ga k1, fromGenKey gb k2)
fromGenKey (Apply f _ ga) k = f (fromGenKey ga k)
fromGenKey _ _ = error "key doesn't match generator"

toGenKey :: Generator a -> a -> GenKey
toGenKey Trivial () = TrivialKey
toGenKey (Choice ga _) (Left a) = LeftKey (toGenKey ga a)
toGenKey (Choice _ gb) (Right b) = RightKey (toGenKey gb b)
toGenKey (Both ga gb) (a, b) = BothKey (toGenKey ga a) (toGenKey gb b)
toGenKey (Apply _ f g) x = toGenKey g (f x)

pickValue :: Generator a -> IO a
pickValue g = fromGenKey g <$> pickGenKey g

values :: Generator a -> [a]
values g = fromGenKey g <$> genKeys g

genOnly :: a -> Generator a
genOnly x = invmap (const x) (const ()) Trivial

genMaybe :: Generator a -> Generator (Maybe a)
genMaybe g =
  invmap
    (either (const Nothing) Just)
    (maybe (Left ()) Right)
    (Choice Trivial g)

genEither :: Generator a -> Generator b -> Generator (Either a b)
genEither = Choice

genPair :: Generator a -> Generator b -> Generator (a, b)
genPair = Both

genCases :: [(a -> Bool, Generator a)] -> Generator a -> Generator a
genCases [] g = g
genCases ((p, g) : gs) fallthrough =
  invmap
    (either id id)
    (\x -> if p x then Left x else Right x)
    (Choice g (genCases gs fallthrough))

genRange :: Integral t => (Integer, Integer) -> Generator t
genRange (lo, hi) =
  invmap fromInteger toInteger
    . invmap (+ lo) (subtract lo)
    $ go (hi - lo + 1)
  where
    go n
      | n == 1 = genOnly 0
      | even n =
          invmap
            (\(x, b) -> 2 * x + b)
            (`divMod` 2)
            (Both (go (n `div` 2)) (go 2))
      | otherwise =
          genCases
            [((== 0), genOnly 0)]
            (invmap (+ 1) (subtract 1) (go (n - 1)))

genBoundedIntegral :: forall t. (Bounded t, Integral t) => Generator t
genBoundedIntegral =
  genRange
    ( fromIntegral (minBound :: t),
      fromIntegral (maxBound :: t)
    )

genPositive :: Generator Integer
genPositive =
  genCases
    [((== 1), genOnly 1), (even, invmap (* 2) (`div` 2) genPositive)]
    (invmap (succ . (* 2)) (`div` 2) genPositive)

genNonNegative :: Generator Integer
genNonNegative = genCases [((== 0), genOnly 0)] genPositive

genInteger :: Generator Integer
genInteger =
  genCases
    [((== 0), genOnly 0), ((> 0), genPositive)]
    (invmap negate negate genPositive)

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
  genGenericRep = invmap (\() -> U1) (\U1 -> ()) Trivial

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
