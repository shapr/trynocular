-- | A 'Key' is a plan for building a value.  There are two types of keys used
-- here, though: a total 'Key', and a 'PartialKey', where the latter represents
-- a partial value, some of whose subterms can be undefined.  When both forms
-- are possible, 'GeneralKey' can be used.  In fact, 'Key' is a synonym for
-- 'GeneralKey Identity', and 'PartialKey' is a synonym for 'GeneralKey Maybe'.
--
-- One way to obtain a 'PartialKey' is with 'spy', which observe the strictness
-- of an 'IO' action on a 'Key', producing a 'PartialKey' that expresses which
-- parts of the 'Key' were forced by the action.
module Trynocular.Key
  ( Key,
    keySimilarity,
    PartialKey,
    totalKey,
    partialKeys,
    subsumes,
    GeneralKey,
    KeyF (..),
    spy,
  )
where

import Control.Concurrent (MVar, newMVar, readMVar, swapMVar)
import Data.Functor.Classes (Eq1 (..), Ord1 (..), Show1 (..))
import Data.Functor.Identity (Identity (..))
import System.IO.Unsafe (unsafeInterleaveIO)

-- | A 'GeneralKey' is a key with some context attached to each node,
-- represented by a 'Functor' called @f@.
type GeneralKey f = f (KeyF f)

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

-- | A unique identifier for a value produced by a 'Trynocular.Generator'.  You
-- can think of this as a "plan" that a 'Trynocular.Generator' can follow to
-- produce a specific value.
type Key = GeneralKey Identity

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

-- | A uniquely identifier for a partial value produced by a
-- 'Trynocular.Generator'.
type PartialKey = GeneralKey Maybe

-- | Produces a 'PartialKey' that is actually total.
totalKey :: Key -> PartialKey
totalKey (Identity k) = Just (mapSubkeys totalKey k)

-- | Returns all possible 'PartialKey's that subsume a given 'Key'.
partialKeys :: Key -> [PartialKey]
partialKeys (Identity k) = Nothing : (Just <$> traverseSubkeys partialKeys k)

-- | Determines whether a 'PartialKey' subsumes another 'GeneralKey' (either a
-- 'Key' or a 'PartialKey') subsumes another if any value of the first key
-- agrees with the second except when the first key is partial.
subsumes :: forall f. Foldable f => PartialKey -> GeneralKey f -> Bool
Nothing `subsumes` _ = True
Just key1 `subsumes` key2 = any (go key1) key2
  where
    go :: KeyF Maybe -> KeyF f -> Bool
    go TrivialF TrivialF = True
    go (LeftF k) (LeftF k') = k `subsumes` k'
    go (RightF k) (RightF k') = k `subsumes` k'
    go (BothF k1 k2) (BothF k1' k2') = k1 `subsumes` k1' && k2 `subsumes` k2'
    go _ _ = False

-- A utility function that's useful for recursive conversion functions on
-- keys.
mapSubkeys :: (GeneralKey f1 -> GeneralKey f2) -> KeyF f1 -> KeyF f2
mapSubkeys f k = runIdentity (traverseSubkeys (pure . f) k)

-- A utility function that's useful for recursive effectful conversion functions
-- on keys.
traverseSubkeys ::
  Applicative m =>
  (GeneralKey f1 -> m (GeneralKey f2)) ->
  KeyF f1 ->
  m (KeyF f2)
traverseSubkeys _ TrivialF = pure TrivialF
traverseSubkeys op (LeftF k) = LeftF <$> op k
traverseSubkeys op (RightF k) = RightF <$> op k
traverseSubkeys op (BothF k1 k2) = BothF <$> op k1 <*> op k2

-- A 'GeneralKey' that contains all the information of a 'Key', and also keeps
-- track of whether any given node has been observed.
type ObservableKey = GeneralKey Observable

-- A 'Functor' used to implement 'ObservableKey'.
data Observable a = Observable (MVar Bool) a

-- Given a 'Key', produces an 'ObservableKey' that can keep track of demand.
makeObservable :: Key -> IO ObservableKey
makeObservable (Identity k) =
  Observable <$> newMVar False <*> traverseSubkeys makeObservable k

-- Given an 'ObservableKey', produces a 'PartialKey' that records how much of
-- the key has been demanded.
observe :: ObservableKey -> IO PartialKey
observe (Observable var k) = do
  forced <- readMVar var
  if forced then Just <$> traverseSubkeys observe k else pure Nothing

-- Given an 'ObservableKey', produces a 'Key' that behaves exactly like the
-- original (on which 'makeObservable' was called), but silently tracks demand
-- as it is consumed.
makeSpy :: ObservableKey -> IO Key
makeSpy (Observable var k) = unsafeInterleaveIO $ do
  _ <- swapMVar var True
  Identity <$> traverseSubkeys makeSpy k

-- | Runs an action on a 'Key', and returns the demand on the 'Key' (as a
-- 'PartialKey') in addition to the result of the action.
spy :: Key -> (Key -> IO a) -> IO (PartialKey, a)
spy k action = do
  ok <- makeObservable k
  x <- action =<< makeSpy ok
  (,x) <$> observe ok
