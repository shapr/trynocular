{-# LANGUAGE TypeFamilies #-}

module Trynocular.Standardizer
  ( Standardizer (..),
    NormalStandardizer,
    normalStandardizer,
    BetaStandardizer,
    betaStandardizer,
    CompleteStandardizer,
    emptyCompleteStandardizer,
  )
where

import Data.FingerTree (FingerTree)
import Data.FingerTree qualified as FingerTree
import Data.Maybe (fromMaybe)
import Numeric.SpecFunctions (erfc, incompleteBeta)

-- | A 'Standardizer' is an online method for estimating the percentile of new
-- observations from a data set.
class Standardizer std where
  type Datum std
  percentile :: std -> Datum std -> (std, Double)

-- | A `Standardizer` implementation that assumes the data is normally
-- distributed, and discounts older observations by exponentially down-weighting
-- them based on the given responsiveness.
data NormalStandardizer = NormalStandardizer
  { _nsMean :: Double,
    _nsVariance :: Double,
    _nsResponsiveness :: Double
  }

instance Standardizer NormalStandardizer where
  type Datum NormalStandardizer = Double

  percentile (NormalStandardizer mean variance responsiveness) x =
    ( NormalStandardizer mean' variance' responsiveness,
      1 - erfc (zscore / sqrt 2) / 2
    )
    where
      mean' = responsiveness * x + (1 - responsiveness) * mean
      xvar = (x - mean) ^ (2 :: Int)
      variance' = responsiveness * xvar + (1 - responsiveness) * variance

      zscore = (x - mean') / sqrt variance'

normalStandardizer :: Double -> Double -> Double -> NormalStandardizer
normalStandardizer = NormalStandardizer

-- | A `Standardizer` implementation that assumes the data follows a beta
-- distribution with a discrete component at each end, and discounts older
-- observations by exponentially down-weighting them based on the given
-- responsiveness.
data BetaStandardizer = BetaStandardizer
  { _bsLow :: Double,
    _bsHigh :: Double,
    _bsLowProb :: Double,
    _bsHighProb :: Double,
    _bsScaledMean :: Double,
    _bsScaledVariance :: Double,
    _bsResponsiveness :: Double
  }

instance Standardizer BetaStandardizer where
  type Datum BetaStandardizer = Double

  percentile
    (BetaStandardizer low high lowp highp mean variance responsiveness)
    x
      | x <= low =
          let lowp' = (1 - responsiveness) * lowp + responsiveness
              highp' = ((1 - responsiveness) * highp)
           in ( BetaStandardizer
                  low
                  high
                  lowp'
                  highp'
                  mean
                  variance
                  responsiveness,
                lowp' / 2
              )
      | x >= high =
          let lowp' = ((1 - responsiveness) * lowp)
              highp' = (1 - responsiveness) * highp + responsiveness
           in ( BetaStandardizer
                  low
                  high
                  lowp'
                  highp'
                  mean
                  variance
                  responsiveness,
                1 - highp' / 2
              )
      | otherwise =
          let scaled = (x - low) / (high - low)

              mean' = responsiveness * scaled + (1 - responsiveness) * mean
              xvar = (scaled - mean) ^ (2 :: Int)
              variance' =
                responsiveness * xvar + (1 - responsiveness) * variance

              alpha = ((1 - mean') / variance' - 1 / mean') * mean' * mean'
              beta = alpha * (1 / mean' - 1)

              lowp' = (1 - responsiveness) * lowp
              highp' = (1 - responsiveness) * highp
           in ( BetaStandardizer
                  low
                  high
                  lowp'
                  highp'
                  mean'
                  variance'
                  responsiveness,
                lowp' + (1 - lowp' - highp') * incompleteBeta alpha beta scaled
              )

betaStandardizer ::
  (Double, Double) -> Double -> Double -> Double -> BetaStandardizer
betaStandardizer (low, high) mean variance responsiveness =
  BetaStandardizer
    low
    high
    0
    0
    ((mean - low) / (high - low))
    (variance / (high - low) ^ (2 :: Int))
    responsiveness

-- | A `Standardizer` implementation that makes no assumptions about
-- distribution, and discounts older observations by exponentially
-- down-weighting them based on the given responsiveness.  This uses
-- significantly more memory than the statistical estimators above, but is more
-- versatile.
data CompleteStandardizer a = CompleteStandardizer
  { _csDataPoints :: FingerTree (Maybe (Counted a)) (Counted a),
    _csCurrentWeight :: Double,
    _csResponsiveness :: Double
  }
  deriving (Show)

data Counted a = Counted {countedVal :: a, countedWeight :: Double}
  deriving (Show)

instance Ord a => Semigroup (Counted a) where
  Counted x xs <> Counted y ys = Counted (min x y) (xs + ys)

instance Ord a => FingerTree.Measured (Maybe (Counted a)) (Counted a) where
  measure = Just

emptyCompleteStandardizer :: Ord a => Double -> CompleteStandardizer a
emptyCompleteStandardizer responsiveness =
  CompleteStandardizer FingerTree.empty 1e-50 responsiveness

instance Ord a => Standardizer (CompleteStandardizer a) where
  type Datum (CompleteStandardizer a) = a

  percentile (CompleteStandardizer dataPoints weight responsiveness) x
    | weight > 1e50 =
        percentile
          ( CompleteStandardizer
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
                ( CompleteStandardizer
                    (l <> (Counted y (w + weight) FingerTree.<| r))
                    (weight / (1 - responsiveness))
                    responsiveness,
                  (totalWeight l + (w + weight) / 2)
                    / (weight + totalWeight dataPoints)
                )
            | otherwise ->
                ( CompleteStandardizer
                    ( (l FingerTree.|> Counted y w)
                        <> (Counted x weight FingerTree.<| r)
                    )
                    (weight / (1 - responsiveness))
                    responsiveness,
                  (totalWeight l + w + weight / 2)
                    / (weight + totalWeight dataPoints)
                )
          FingerTree.OnLeft ->
            ( CompleteStandardizer
                (Counted x weight FingerTree.<| dataPoints)
                (weight / (1 - responsiveness))
                responsiveness,
              weight / (2 * (weight + totalWeight dataPoints))
            )
          _ -> error "CompleteStandardizer percentile: invariant violation"
    where
      totalWeight = maybe 0 countedWeight . FingerTree.measure