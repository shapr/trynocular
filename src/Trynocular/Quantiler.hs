{-# LANGUAGE TypeFamilies #-}

module Trynocular.Quantiler
  ( Quantiler (..),
    NormalQuantiler,
    normalQuantiler,
    BetaQuantiler,
    betaQuantiler,
    CompleteQuantiler,
    emptyCompleteQuantiler,
  )
where

import Data.FingerTree (FingerTree)
import Data.FingerTree qualified as FingerTree
import Data.Maybe (fromMaybe)
import Numeric.SpecFunctions (erfc, incompleteBeta)

class Quantiler q where
  type Datum q
  quantile :: q -> Datum q -> (q, Double)

data NormalQuantiler = NormalQuantiler
  { _nqMean :: Double,
    _nqVariance :: Double,
    _nqResponsiveness :: Double
  }

instance Quantiler NormalQuantiler where
  type Datum NormalQuantiler = Double
  quantile (NormalQuantiler mean variance responsiveness) x =
    (NormalQuantiler mean' variance' responsiveness, erfc (zscore / sqrt 2) / 2)
    where
      mean' = responsiveness * x + (1 - responsiveness) * mean
      xvar = (x - mean) ^ (2 :: Int)
      variance' = responsiveness * xvar + (1 - responsiveness) * variance

      zscore = (x - mean') / sqrt variance'

normalQuantiler :: Double -> Double -> Double -> NormalQuantiler
normalQuantiler = NormalQuantiler

data BetaQuantiler = BetaQuantiler
  { _bqLow :: Double,
    _bqHigh :: Double,
    _bqScaledMean :: Double,
    _bqScaledVariance :: Double,
    _bqResponsiveness :: Double
  }

instance Quantiler BetaQuantiler where
  type Datum BetaQuantiler = Double
  quantile (BetaQuantiler low high mean variance responsiveness) x =
    (BetaQuantiler low high mean' variance' responsiveness, result)
    where
      scaled = (x - low) / (high - low)

      mean' = responsiveness * scaled + (1 - responsiveness) * mean
      xvar = (scaled - mean) ^ (2 :: Int)
      variance' = responsiveness * xvar + (1 - responsiveness) * variance

      alpha = ((1 - mean') / variance' - 1 / mean') * mean' * mean'
      beta = alpha * (1 / mean' - 1)

      result
        | scaled < 0 = 0
        | scaled > 1 = 1
        | otherwise = 1 - incompleteBeta alpha beta scaled

betaQuantiler :: (Double, Double) -> Double -> Double -> Double -> BetaQuantiler
betaQuantiler (low, high) mean variance responsiveness =
  BetaQuantiler
    low
    high
    ((mean - low) / (high - low))
    (variance / (high - low) ^ (2 :: Int))
    responsiveness

data CompleteQuantiler a = CompleteQuantiler
  { _dataPoints :: FingerTree (Maybe (Counted a)) (Counted a)
  }
    deriving Show

data Counted a = Counted {countedVal :: a, countedNum :: Int}
    deriving Show

instance Ord a => Semigroup (Counted a) where
  Counted x xs <> Counted y ys = Counted (min x y) (xs + ys)

instance Ord a => FingerTree.Measured (Maybe (Counted a)) (Counted a) where
  measure = Just

emptyCompleteQuantiler :: Ord a => CompleteQuantiler a
emptyCompleteQuantiler = CompleteQuantiler FingerTree.empty

instance Ord a => Quantiler (CompleteQuantiler a) where
  type Datum (CompleteQuantiler a) = a
  quantile (CompleteQuantiler dataPoints) x =
    case FingerTree.search
      (\_ r -> fromMaybe True ((<) <$> Just x <*> fmap countedVal r))
      dataPoints of
      FingerTree.Position l (Counted y n) r
        | x == y ->
            ( CompleteQuantiler (l <> (Counted y (n + 1) FingerTree.<| r)),
              (fromIntegral (size l) + (fromIntegral n + 1) / 2)
                / fromIntegral (1 + size dataPoints)
            )
        | otherwise ->
            ( CompleteQuantiler
                ( (l FingerTree.|> Counted y n)
                    <> (Counted x 1 FingerTree.<| r)
                ),
              (fromIntegral (size l + n) + 0.5)
                / fromIntegral (1 + size dataPoints)
            )
      FingerTree.OnLeft ->
        ( CompleteQuantiler (Counted x 1 FingerTree.<| dataPoints),
          0.5 / fromIntegral (1 + size dataPoints)
        )
      _ -> error "CompleteQuantiler quantile: invariant violation"
    where size = maybe 0 countedNum . FingerTree.measure