{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ExplicitNamespaces #-}

-- | The 'Generable' type class contains types that have a default 'Generator'
-- capable of generating any value of that type.
module Trynocular.Generable (Generable (..), GenericGenerable (..)) where

import Data.Char (chr)
import Data.Functor.Alt (Alt (..))
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Generics
  ( Generic (..),
    K1 (K1),
    M1 (M1),
    U1 (..),
    type (:*:) (..),
    type (:+:) (..),
  )
import Trynocular.Generator
import Unsafe.Coerce

-- | A 'Generator' that produces an arbitrary 'Integer'.
genInteger :: Generator Integer
genInteger = pure 0 <!> genPositive <!> negate <$> genPositive

-- | A 'Generator' that produces an arbitrary 'Float'.
genFloat :: Generator Float
genFloat
  | isIEEE (undefined :: Float)
      && floatRadix (undefined :: Float) == 2
      && floatDigits (undefined :: Float) == 24
      && floatRange (undefined :: Float) == (-125, 128) =
      unsafeCoerce <$> (genAny @Word32)
  | otherwise = error "Float is not IEEE single-precision on this platform!"

-- | A 'Generator' that produces an arbitrary 'Double'.
genDouble :: Generator Double
genDouble
  | isIEEE (undefined :: Double)
      && floatRadix (undefined :: Double) == 2
      && floatDigits (undefined :: Double) == 53
      && floatRange (undefined :: Double) == (-1021, 1024) =
      unsafeCoerce <$> (genAny @Word64)
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

instance Generable Char where genAny = fmap chr (genRange (0, 0x10FFFF))

instance Generable Word8 where genAny = genBoundedIntegral

instance Generable Word16 where genAny = genBoundedIntegral

instance Generable Word32 where genAny = genBoundedIntegral

instance Generable Word64 where genAny = genBoundedIntegral

instance Generable Word where genAny = genBoundedIntegral

instance Generable Int8 where genAny = genBoundedIntegral

instance Generable Int16 where genAny = genBoundedIntegral

instance Generable Int32 where genAny = genBoundedIntegral

instance Generable Int64 where genAny = genBoundedIntegral

instance Generable Int where genAny = genBoundedIntegral

instance Generable Integer where genAny = genInteger

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
  genGenericRep = pure U1

instance
  (GenericGenerable f, GenericGenerable g) =>
  GenericGenerable (f :+: g)
  where
  genGenericRep = L1 <$> genGenericRep <!> R1 <$> genGenericRep

instance
  (GenericGenerable f, GenericGenerable g) =>
  GenericGenerable (f :*: g)
  where
  genGenericRep = (:*:) <$> genGenericRep <*> genGenericRep

instance Generable a => GenericGenerable (K1 i a) where
  genGenericRep = K1 <$> genAny

instance GenericGenerable f => GenericGenerable (M1 i t f) where
  genGenericRep = M1 <$> genGenericRep

genGeneric :: (Generic a, GenericGenerable (Rep a)) => Generator a
genGeneric = to <$> genGenericRep
