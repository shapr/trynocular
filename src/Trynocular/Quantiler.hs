{-# LANGUAGE TypeFamilies #-}

module Trynocular.Quantiler (Quantiler (..), NormalQuantiler, normalQuantiler, BetaQuantiler, betaQuantiler) where

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
      zscore = (x - mean) / sqrt variance
      mean' = responsiveness * x + (1 - responsiveness) * mean
      xvar = (x - mean) ^ (2 :: Int)
      variance' = responsiveness * xvar + (1 - responsiveness) * variance

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

      alpha = ((1 - mean) / variance - 1 / mean) * mean * mean
      beta = alpha * (1 / mean - 1)

      result
        | scaled < 0 = 0
        | scaled > 1 = 1
        | otherwise = 1 - incompleteBeta alpha beta scaled

      mean' = responsiveness * scaled + (1 - responsiveness) * mean
      xvar = (scaled - mean) ^ (2 :: Int)
      variance' = responsiveness * xvar + (1 - responsiveness) * variance

betaQuantiler :: (Double, Double) -> Double -> Double -> Double -> BetaQuantiler
betaQuantiler (low, high) mean variance responsiveness =
  BetaQuantiler
    low
    high
    ((mean - low) / (high - low))
    (variance / (high - low) ^ (2 :: Int))
    responsiveness
