{-# LANGUAGE GADTs #-}

module Main where

import Data.Bifunctor (second)
import Data.Bool (bool)
import Data.Either
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Universe.Helpers ((+*+), (+++))
import Data.Word (Word16, Word32, Word64, Word8)
import System.Random

main :: IO ()
main = putStrLn "Hello, Haskell!"

data Generator a where
  Trivial :: Generator ()
  Choice :: Generator a -> Generator b -> Generator (Either a b)
  Both :: Generator a -> Generator b -> Generator (a, b)
  Apply :: (a -> b) -> Generator a -> Generator b

-- | fmap id should be id, fmap id here will be Apply id ? which is a different
-- term than the input.  If constructors are not exported, the Functor law holds
-- up to observable behavior.
instance Functor Generator where
  fmap :: (a -> b) -> Generator a -> Generator b
  fmap = Apply

instance Applicative Generator where
  pure :: a -> Generator a
  pure a = Apply (const a) Trivial
  (<*>) :: Generator (d -> e) -> Generator d -> Generator e
  a <*> b = Apply (uncurry ($)) (Both a b)

data GenKey = TrivialKey | ChoiceKey Bool GenKey | BothKey GenKey GenKey
  deriving (Eq, Ord, Show)

serializeKey :: GenKey -> [Bool]
serializeKey TrivialKey = []
serializeKey (ChoiceKey b k) = b : serializeKey k
serializeKey (BothKey k1 k2) = serializeKey k1 ++ serializeKey k2

deserializeKey :: Generator a -> [Bool] -> (GenKey, [Bool])
deserializeKey Trivial xs = (TrivialKey, xs)
deserializeKey (Choice a _) (True : xs) =
  let (k, xs') = deserializeKey a xs in (ChoiceKey True k, xs')
deserializeKey (Choice _ b) (False : xs) =
  let (k, xs') = deserializeKey b xs in (ChoiceKey False k, xs')
deserializeKey (Both a b) xs =
  let (k1, xs') = deserializeKey a xs
   in let (k2, xs'') = deserializeKey b xs'
       in (BothKey k1 k2, xs'')
deserializeKey (Apply _ a) xs = deserializeKey a xs
deserializeKey (Choice _ _) [] = error "serialized key too short"

runGenerator :: Generator a -> IO (GenKey, a)
runGenerator Trivial = pure (TrivialKey, ())
runGenerator (Choice ga gb) = do
  b <- randomIO
  if b
    then (\(k, x) -> (ChoiceKey True k, Left x)) <$> runGenerator ga
    else (\(k, x) -> (ChoiceKey False k, Right x)) <$> runGenerator gb
runGenerator (Both ga gb) =
  (\(k1, x1) (k2, x2) -> (BothKey k1 k2, (x1, x2)))
    <$> runGenerator ga <*> runGenerator gb
runGenerator (Apply f ga) = (\(k, x) -> (k, f x)) <$> runGenerator ga

enumGenerator :: Generator a -> [(GenKey, a)]
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

unitGen :: Generator ()
unitGen = Trivial

boolGen :: Generator Bool
boolGen = Apply isRight (Choice Trivial Trivial)

maybeGen :: Generator a -> Generator (Maybe a)
maybeGen g = either (const Nothing) Just <$> Choice Trivial g

eitherGen :: Generator a -> Generator b -> Generator (Either a b)
eitherGen = Choice

pairGen :: Generator a -> Generator b -> Generator (a, b)
pairGen = Both

listGen :: Generator a -> Generator [a]
listGen g =
  either (const []) (uncurry (:))
    <$> Choice Trivial (Both g (listGen g))

nbitsGen :: Integral t => Int -> Generator t
nbitsGen 0 = pure 0
nbitsGen n = (\x y -> x * 2 + bool 0 1 y) <$> nbitsGen (n - 1) <*> boolGen

word8Gen :: Generator Word8
word8Gen = nbitsGen 8

word16Gen :: Generator Word16
word16Gen = nbitsGen 16

word32Gen :: Generator Word32
word32Gen = nbitsGen 32

word64Gen :: Generator Word64
word64Gen = nbitsGen 64

int8Gen :: Generator Int8
int8Gen = nbitsGen 16

int16Gen :: Generator Int16
int16Gen = nbitsGen 16

int32Gen :: Generator Int32
int32Gen = nbitsGen 32

int64Gen :: Generator Int64
int64Gen = nbitsGen 64

integerGen :: Generator Integer
integerGen =
  either (const 0) (either negate id)
    <$> Choice Trivial (Choice posIntegerGen posIntegerGen)

posIntegerGen :: Generator Integer
posIntegerGen =
  either (const 1) (either (* 2) ((+ 1) . (* 2)))
    <$> Choice Trivial (Choice posIntegerGen posIntegerGen)

class Generable a where gen :: Generator a

instance Generable () where gen = unitGen

instance Generable Bool where gen = boolGen

instance Generable Word8 where gen = word8Gen

instance Generable Word16 where gen = word16Gen

instance Generable Word32 where gen = word32Gen

instance Generable Word64 where gen = word64Gen

instance Generable Int8 where gen = int8Gen

instance Generable Int16 where gen = int16Gen

instance Generable Int32 where gen = int32Gen

instance Generable Int64 where gen = int64Gen

instance Generable Integer where gen = integerGen

instance Generable a => Generable (Maybe a) where gen = maybeGen gen

instance (Generable a, Generable b) => Generable (Either a b) where
  gen = eitherGen gen gen

instance (Generable a, Generable b) => Generable (a, b) where
  gen = pairGen gen gen

instance Generable a => Generable [a] where
  gen = listGen gen
