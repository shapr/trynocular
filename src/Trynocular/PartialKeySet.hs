module Trynocular.PartialKeySet
  ( PartialKeySet,
    empty,
    singleton,
    fromList,
    insert,
    member,
  )
where

import Data.Foldable (Foldable (..))
import Data.Functor.Identity (Identity (..))
import Trynocular.Key (Key, KeyF (..), PartialKey)

newtype PartialKeySet = PartialKeySet (PKTrie ())

empty :: PartialKeySet
empty = PartialKeySet EmptyPKTrie

singleton :: PartialKey -> PartialKeySet
singleton k = insert k empty

fromList :: [PartialKey] -> PartialKeySet
fromList = foldr insert empty

insert :: PartialKey -> PartialKeySet -> PartialKeySet
insert pk (PartialKeySet pkt) = PartialKeySet (insertPKTrie pk () pkt)

member :: Key -> PartialKeySet -> Bool
member k (PartialKeySet pkt) = not (null (lookupPKTrie k pkt))

data PKTrie a = EmptyPKTrie | NonEmptyPKTrie (NEPKTrie a)

insertPKTrie :: PartialKey -> a -> PKTrie a -> PKTrie a
insertPKTrie pk x EmptyPKTrie =
  NonEmptyPKTrie (singletonNEPKTrie pk x)
insertPKTrie pk x (NonEmptyPKTrie nepkt) =
  NonEmptyPKTrie (insertNEPKTrie pk x nepkt)

lookupPKTrie :: Key -> PKTrie a -> [a]
lookupPKTrie _ EmptyPKTrie = []
lookupPKTrie k (NonEmptyPKTrie nepkt) = lookupNEPKTrie k nepkt

newtype NEPKTrie a = NEPKTrie (SumTrie Identity KeyFTrie a)

singletonNEPKTrie :: PartialKey -> a -> NEPKTrie a
singletonNEPKTrie Nothing x = NEPKTrie (LTrie (Identity x))
singletonNEPKTrie (Just k) x = NEPKTrie (RTrie (singletonKeyFTrie k x))

insertNEPKTrie :: PartialKey -> a -> NEPKTrie a -> NEPKTrie a
insertNEPKTrie k x = alterNEPKTrie k x (const x)

alterNEPKTrie :: PartialKey -> a -> (a -> a) -> NEPKTrie a -> NEPKTrie a
alterNEPKTrie Nothing new old (NEPKTrie t) =
  NEPKTrie (onLeft (Identity new) (\(Identity x) -> Identity (old x)) t)
alterNEPKTrie (Just k) new old (NEPKTrie t) =
  NEPKTrie (onRight (singletonKeyFTrie k new) (alterKeyFTrie k new old) t)

lookupNEPKTrie :: Key -> NEPKTrie a -> [a]
lookupNEPKTrie (Identity k) (NEPKTrie t) = fromBoth toList (lookupKeyFTrie k) t

data SumTrie f g a = LTrie (f a) | RTrie (g a) | BTrie (f a) (g a)

onLeft :: f a -> (f a -> f a) -> SumTrie f g a -> SumTrie f g a
onLeft _ old (LTrie t) = LTrie (old t)
onLeft new _ (RTrie t) = BTrie new t
onLeft _ old (BTrie l r) = BTrie (old l) r

onRight :: g a -> (g a -> g a) -> SumTrie f g a -> SumTrie f g a
onRight new _ (LTrie t) = BTrie t new
onRight _ old (RTrie t) = RTrie (old t)
onRight _ old (BTrie l r) = BTrie l (old r)

fromLeft :: Monoid m => (f a -> m) -> SumTrie f g a -> m
fromLeft f (LTrie t) = f t
fromLeft f (BTrie l _) = f l
fromLeft _ _ = mempty

fromRight :: Monoid m => (g a -> m) -> SumTrie f g a -> m
fromRight f (RTrie t) = f t
fromRight f (BTrie _ r) = f r
fromRight _ _ = mempty

fromBoth :: Semigroup s => (f a -> s) -> (g a -> s) -> SumTrie f g a -> s
fromBoth f _ (LTrie t) = f t
fromBoth _ g (RTrie t) = g t
fromBoth f g (BTrie l r) = f l <> g r

newtype ProductTrie f g a = ProductTrie (f (g a))

data KeyFTrie a
  = TrivialTrie a
  | ChoiceTrie (SumTrie NEPKTrie NEPKTrie a)
  | BothTrie (ProductTrie NEPKTrie NEPKTrie a)

singletonKeyFTrie :: KeyF Maybe -> a -> KeyFTrie a
singletonKeyFTrie TrivialF x = TrivialTrie x
singletonKeyFTrie (LeftF k) x = ChoiceTrie (LTrie (singletonNEPKTrie k x))
singletonKeyFTrie (RightF k) x = ChoiceTrie (RTrie (singletonNEPKTrie k x))
singletonKeyFTrie (BothF k1 k2) x =
  BothTrie (ProductTrie (singletonNEPKTrie k1 (singletonNEPKTrie k2 x)))

alterKeyFTrie :: KeyF Maybe -> a -> (a -> a) -> KeyFTrie a -> KeyFTrie a
alterKeyFTrie TrivialF _ old (TrivialTrie x) = TrivialTrie (old x)
alterKeyFTrie (LeftF k) new old (ChoiceTrie t) =
  ChoiceTrie (onLeft (singletonNEPKTrie k new) (alterNEPKTrie k new old) t)
alterKeyFTrie (RightF k) new old (ChoiceTrie t) =
  ChoiceTrie (onRight (singletonNEPKTrie k new) (alterNEPKTrie k new old) t)
alterKeyFTrie (BothF k1 k2) new old (BothTrie (ProductTrie t)) =
  BothTrie
    ( ProductTrie
        ( alterNEPKTrie
            k1
            (singletonNEPKTrie k2 new)
            (alterNEPKTrie k2 new old)
            t
        )
    )
alterKeyFTrie _ _ _ _ = error "Inconsistent keys"

lookupKeyFTrie :: KeyF Identity -> KeyFTrie a -> [a]
lookupKeyFTrie TrivialF (TrivialTrie x) = [x]
lookupKeyFTrie (LeftF k) (ChoiceTrie t) = fromLeft (lookupNEPKTrie k) t
lookupKeyFTrie (RightF k) (ChoiceTrie t) = fromRight (lookupNEPKTrie k) t
lookupKeyFTrie (BothF k1 k2) (BothTrie (ProductTrie t)) =
  concatMap (lookupNEPKTrie k2) (lookupNEPKTrie k1 t)
lookupKeyFTrie _ _ = []
