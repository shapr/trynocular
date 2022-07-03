module Trynocular.TestHarness where

import Control.Monad (when)
import Trynocular.Generator
import Trynocular.Key
import Trynocular.PartialKeySet (PartialKeySet)
import Trace.Hpc.Reflect (examineTix)
import Trace.Hpc.Tix
import Trynocular.PartialKeySet qualified as PartialKeySet

data CoverageExpectation = CoverageExpectation
  { coverageMean :: Double,
    coverageVariance :: Double
  }

data TestState
  = forall params.
    TestState
      Int
      (Generator params)
      (params -> IO ())
      PartialKeySet
      CoverageExpectation

initialTestState ::
  forall params.
  Generator params ->
  (params -> IO ()) ->
  TestState
initialTestState generator action =
  TestState
    0
    generator
    action
    PartialKeySet.empty
    (CoverageExpectation {coverageMean = 0.5, coverageVariance = 1})

updateCoverageExpectation ::
  Double -> CoverageExpectation -> CoverageExpectation
updateCoverageExpectation observed (CoverageExpectation mean var) =
  CoverageExpectation mean' var'
  where
    mean' = alpha * observed + (1 - alpha) * mean
    observedVar = (observed - mean) ^ (2 :: Int)
    var' = alpha * observedVar + (1 - alpha) * var
    alpha = 0.1

updateGenerator :: Double -- ^ stddevs above or below
                -> PartialKey -- ^ demanded portion of the key
                -> Generator a -- ^ generator to update
                -> Generator a -- ^ new updated generator
updateGenerator _score _pkey gen = gen

testHarness :: TestState -> IO ()
testHarness state@(TestState n generator action usedKeys coverage) = do
  key <- pickKey generator
  if key `PartialKeySet.member` usedKeys
    then testHarness state
    else do
      (pkey, ((), observedCoverage)) <- spy key (tixCountWrapper . action . fromKey generator)
      let score = (observedCoverage - coverageMean coverage)
            / sqrt (coverageVariance coverage)
      let generator' = updateGenerator score pkey generator
      let coverage' = updateCoverageExpectation (observedCoverage) coverage
      let usedKeys' = PartialKeySet.insert pkey usedKeys
      let state' = TestState (n + 1) generator' action usedKeys' coverage'
      when (shouldContinue state') $ testHarness state'

shouldContinue :: TestState -> Bool
shouldContinue (TestState n _ _ _ _) = n < 1000

tixCountWrapper :: IO a -> IO (a, Double)
tixCountWrapper a = do
  preTix <- examineTix
  a' <- a
  afterTix <- examineTix
  pure (a', realToFrac (additionalTix preTix afterTix) / realToFrac (tixModuleSize preTix))

additionalTix :: Tix -> Tix -> Int
additionalTix old new = tixModuleCount new - tixModuleCount old

-- How many regions were executed at least once for this module?
tixCount :: TixModule -> Int
tixCount (TixModule _ _ _ regions) = sum $ 1 <$ filter (> 0) regions

-- How many regions were executed at least once for all these modules?
tixModuleCount :: Tix -> Int
tixModuleCount (Tix ms) = sum $ map tixCount ms

tixModuleSize :: Tix -> Int
tixModuleSize (Tix ms) = sum $ tixSize <$> ms

tixSize :: TixModule -> Int
tixSize (TixModule _ _ s _) = s
