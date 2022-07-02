module Trynocular.Key
  ( Key,
    keySimilarity,
    PartialKey,
    totalKey,
    isPartially,
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

-- | A unique identifier for a total value produced by a 'Generator'.  You can
-- think of this as a "plan" that a 'Generator' can follow to produce a specific
-- value.
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

-- | A uniquely identifier for a partial value produced by a 'Generator'.
type PartialKey = GeneralKey Maybe

-- A 'GeneralKey' that contains all the information of a 'Key', and also keeps
-- track of whether any given node has been observed.
type ObservableKey = GeneralKey Observable

-- A 'Functor' used to implement 'ObservableKey'.
data Observable a = Observable (MVar Bool) a

totalKey :: Key -> PartialKey
totalKey (Identity k) = Just (onSubkeys totalKey k)

isPartially :: PartialKey -> Key -> Bool
isPartially Nothing _ = True
isPartially (Just TrivialF) (Identity TrivialF) = True
isPartially (Just (LeftF k)) (Identity (LeftF k')) = isPartially k k'
isPartially (Just (RightF k)) (Identity (RightF k')) = isPartially k k'
isPartially (Just (BothF k1 k2)) (Identity (BothF k1' k2')) =
  isPartially k1 k1' && isPartially k2 k2'
isPartially _ _ = False

-- A utility function that's useful for recursive conversion functions on
-- keys.
onSubkeysM ::
  Monad m => (GeneralKey f1 -> m (GeneralKey f2)) -> KeyF f1 -> m (KeyF f2)
onSubkeysM _ TrivialF = pure TrivialF
onSubkeysM op (LeftF k) = LeftF <$> op k
onSubkeysM op (RightF k) = RightF <$> op k
onSubkeysM op (BothF k1 k2) = BothF <$> op k1 <*> op k2

onSubkeys :: (GeneralKey f1 -> GeneralKey f2) -> KeyF f1 -> KeyF f2
onSubkeys f k = runIdentity (onSubkeysM (pure . f) k)

-- Given a 'Key', produces an 'ObservableKey' that can keep track of demand.
makeObservable :: Key -> IO ObservableKey
makeObservable (Identity k) = do
  var <- newMVar False
  Observable var <$> onSubkeysM makeObservable k

-- Given an 'ObservableKey', produces a 'PartialKey' that records how much of
-- the key has been demanded.
observe :: ObservableKey -> IO PartialKey
observe (Observable var k) = do
  forced <- readMVar var
  if forced then Just <$> onSubkeysM observe k else pure Nothing

-- Given an 'ObservableKey', produces a 'Key' that behaves exactly like the
-- original (on which 'makeObservable' was called), but silently tracks demand
-- as it is consumed.
makeSpy :: ObservableKey -> IO Key
makeSpy (Observable var k) = unsafeInterleaveIO $ do
  _ <- swapMVar var True
  Identity <$> onSubkeysM makeSpy k

-- | Runs an action on a 'Key', and returns the demand on the 'Key' (as a
-- 'PartialKey') in addition to the result of the action.
spy :: Key -> (Key -> IO a) -> IO (PartialKey, a)
spy k action = do
  ok <- makeObservable k
  x <- action =<< makeSpy ok
  (,x) <$> observe ok
