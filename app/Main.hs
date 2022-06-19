{-# LANGUAGE GADTs #-}

module Main where

import Data.Either
import System.Random

main :: IO ()
main = putStrLn "Hello, Haskell!"

data Generator a where
  Trivial :: Generator ()
  Choice :: Generator a -> Generator b -> Generator (Either a b) -- if a and b are the same, you get Either a a which is easy to collapse into a
  Both :: Generator a -> Generator b -> Generator (a, b)
  Apply :: (a -> b) -> Generator a -> Generator b

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

runGenerator :: Generator a -> IO a
runGenerator Trivial = pure ()
runGenerator (Choice ga gb) = do
  b <- randomIO
  if b
    then Left <$> runGenerator ga
    else Right <$> runGenerator gb
runGenerator (Both ga gb) = (,) <$> runGenerator ga <*> runGenerator gb
runGenerator (Apply f ga) = fmap f $ runGenerator ga

-- | generator for Bool
boolGen :: Generator Bool
boolGen = Apply isLeft (Choice Trivial Trivial)

maybeGen :: Generator a -> Generator (Maybe a)
maybeGen g = either (const Nothing) Just <$> Choice Trivial g

-- eitherGen :: Generator g -> Generator h -> Generator (Either g h)
-- eitherGen g h = Choice (Left <$> g) (Right <$> h)

-- pairGen :: Generator a -> Generator b -> Generator (a, b)
-- pairGen a b = (,) <$> a <*> b -- don't want forcing the b to force the a
-- -- how many octects to parse for this string? 08 >>= \c -> read c octets
