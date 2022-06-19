{-# LANGUAGE GADTs #-}

module Main where

import Data.Bool (bool)
import Data.Either
import Data.Int (Int8)
import Data.Universe.Helpers ((+*+), (+++))
import Data.Word (Word16, Word8)
import System.Random

main :: IO ()
main = putStrLn "Hello, Haskell!"

data Generator a where
  Trivial :: Generator ()
  Choice :: Generator a -> Generator b -> Generator (Either a b) -- if a and b are the same, you get Either a a which is easy to collapse into a
  Both :: Generator a -> Generator b -> Generator (a, b)
  Apply :: (a -> b) -> Generator a -> Generator b

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

reproduceGen :: Generator a -> GenKey -> a
reproduceGen Trivial TrivialKey = ()
reproduceGen (Choice ga gb) (ChoiceKey b k) = if b then Left (reproduceGen ga k) else Right (reproduceGen gb k)
reproduceGen (Both ga gb) (BothKey k1 k2) = (reproduceGen ga k1, reproduceGen gb k2)
reproduceGen (Apply f ga) k = f (reproduceGen ga k)
reproduceGen _ _ = error "key doesn't match generator"

-- | fmap id should be id, fmap id here will be Apply id ? which is a different term than the input
-- will give you a function that always produces a more complex value because it wraps it in Apply
-- if constructors are not exported, the Functor law holds up to observable behavior
instance Functor Generator where
  fmap :: (a -> b) -> Generator a -> Generator b
  fmap = Apply

instance Applicative Generator where
  pure :: a -> Generator a
  pure a = Apply (const a) Trivial
  (<*>) :: Generator (d -> e) -> Generator d -> Generator e
  a <*> b = Apply (uncurry ($)) (Both a b)

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

enumGenerator :: Generator a -> [a]
enumGenerator Trivial = [()]
enumGenerator (Choice ga gb) =
  (Left <$> enumGenerator ga) +++ (Right <$> enumGenerator gb)
enumGenerator (Both ga gb) = (enumGenerator ga) +*+ (enumGenerator gb)
enumGenerator (Apply f ga) = map f $ enumGenerator ga

-- | generator for Bool
boolGen :: Generator Bool
boolGen = Apply isRight (Choice Trivial Trivial)

maybeGen :: Generator a -> Generator (Maybe a)
maybeGen g = either (const Nothing) Just <$> Choice Trivial g

eitherGen :: Generator a -> Generator b -> Generator (Either a b)
eitherGen = Choice

pairGen :: Generator a -> Generator b -> Generator (a, b)
pairGen = Both

nbits :: Integral t => Int -> Generator t
nbits 0 = pure 0
nbits n = (\x y -> x * 2 + y) <$> nbits (n - 1) <*> oneBit
  where
    oneBit = bool 0 1 <$> boolGen

word8Gen :: Generator Word8
word8Gen = nbits 8

word16Gen :: Generator Word16
word16Gen = nbits 16

int8Gen :: Generator Int8
int8Gen = nbits 8

integerGen :: Generator Integer
integerGen = either (const 0) (either negate id) <$> (Choice Trivial (Choice natGen natGen))

natGen :: Generator Integer -- strictly positive
natGen =
  either (const 1) (either (* 2) ((+ 1) . (* 2)))
    <$> Choice Trivial (Choice natGen natGen)

infiniteListGen :: Generator [()]
infiniteListGen = Apply (either id (const [])) (Choice infiniteListGen Trivial)
