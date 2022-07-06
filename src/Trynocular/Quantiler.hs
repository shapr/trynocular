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

-- | A 'Quantiler' is an online method for estimating the quantile of new
-- observations from a data set.
class Quantiler q where
  type Datum q
  quantile :: q -> Datum q -> (q, Double)

-- | A `Quantiler` implementation that assumes the data is normally distributed,
-- and discounts older observations by exponentially down-weighting them based
-- on the given responsiveness.
data NormalQuantiler = NormalQuantiler
  { _nqMean :: Double,
    _nqVariance :: Double,
    _nqResponsiveness :: Double
  }

instance Quantiler NormalQuantiler where
  type Datum NormalQuantiler = Double

  quantile (NormalQuantiler mean variance responsiveness) x =
    ( NormalQuantiler mean' variance' responsiveness,
      1 - erfc (zscore / sqrt 2) / 2
    )
    where
      mean' = responsiveness * x + (1 - responsiveness) * mean
      xvar = (x - mean) ^ (2 :: Int)
      variance' = responsiveness * xvar + (1 - responsiveness) * variance

      zscore = (x - mean') / sqrt variance'

normalQuantiler :: Double -> Double -> Double -> NormalQuantiler
normalQuantiler = NormalQuantiler

-- | A `Quantiler` implementation that assumes the data follows a beta
-- distribution, and discounts older observations by exponentially
-- down-weighting them based on the given responsiveness.
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
        | otherwise = incompleteBeta alpha beta scaled

betaQuantiler :: (Double, Double) -> Double -> Double -> Double -> BetaQuantiler
betaQuantiler (low, high) mean variance responsiveness =
  BetaQuantiler
    low
    high
    ((mean - low) / (high - low))
    (variance / (high - low) ^ (2 :: Int))
    responsiveness

-- | A `Quantiler` implementation that makes no assumptions about distribution,
-- and discounts older observations by exponentially down-weighting them based
-- on the given responsiveness.  This uses significantly more memory than the
-- statistical estimators above, but is more versatile.
data CompleteQuantiler a = CompleteQuantiler
  { _cqDataPoints :: FingerTree (Maybe (Counted a)) (Counted a),
    _cqCurrentWeight :: Double,
    _cqResponsiveness :: Double
  }
  deriving (Show)

data Counted a = Counted {countedVal :: a, countedWeight :: Double}
  deriving (Show)

instance Ord a => Semigroup (Counted a) where
  Counted x xs <> Counted y ys = Counted (min x y) (xs + ys)

instance Ord a => FingerTree.Measured (Maybe (Counted a)) (Counted a) where
  measure = Just

emptyCompleteQuantiler :: Ord a => Double -> CompleteQuantiler a
emptyCompleteQuantiler responsiveness =
  CompleteQuantiler FingerTree.empty 1e-50 responsiveness

instance Ord a => Quantiler (CompleteQuantiler a) where
  type Datum (CompleteQuantiler a) = a

  quantile (CompleteQuantiler dataPoints weight responsiveness) x
    | weight > 1e50 =
        quantile
          ( CompleteQuantiler
              ( FingerTree.fmap'
                  (\(Counted y w) -> Counted y (w / weight * 1e-50))
                  dataPoints
              )
              1e-50
              responsiveness
          )
          x
    | otherwise =
        case FingerTree.search
          (\_ r -> fromMaybe True ((<) <$> Just x <*> fmap countedVal r))
          dataPoints of
          FingerTree.Position l (Counted y w) r
            | x == y ->
                ( CompleteQuantiler
                    (l <> (Counted y (w + weight) FingerTree.<| r))
                    (weight / (1 - responsiveness))
                    responsiveness,
                  (totalWeight l + (w + weight) / 2)
                    / (weight + totalWeight dataPoints)
                )
            | otherwise ->
                ( CompleteQuantiler
                    ( (l FingerTree.|> Counted y w)
                        <> (Counted x weight FingerTree.<| r)
                    )
                    (weight / (1 - responsiveness))
                    responsiveness,
                  (totalWeight l + w + weight / 2)
                    / (weight + totalWeight dataPoints)
                )
          FingerTree.OnLeft ->
            ( CompleteQuantiler
                (Counted x weight FingerTree.<| dataPoints)
                (weight / (1 - responsiveness))
                responsiveness,
              weight / (2 * (weight + totalWeight dataPoints))
            )
          _ -> error "CompleteQuantiler quantile: invariant violation"
    where
      totalWeight = maybe 0 countedWeight . FingerTree.measure