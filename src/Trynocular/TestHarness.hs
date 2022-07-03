module Trynocular.TestHarness where

import Control.Monad (when)
import Trynocular.Generator
import Trynocular.Key
import Trynocular.PartialKeySet (PartialKeySet)
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
    (CoverageExpectation {coverageMean = 0.5, coverageVariance = 0.25})

updateCoverageExpectation ::
  Double -> CoverageExpectation -> CoverageExpectation
updateCoverageExpectation observed (CoverageExpectation mean var) =
  CoverageExpectation mean' var'
  where
    mean' = alpha * observed + (1 - alpha) * mean
    observedVar = (observed - mean) ^ (2 :: Int)
    var' = alpha * observedVar + (1 - alpha) * var
    alpha = 0.1

updateGenerator :: Double -> PartialKey -> Generator a -> Generator a
updateGenerator _ _ gen = gen -- TODO

testHarness :: TestState -> IO ()
testHarness state@(TestState n generator action usedKeys coverage) = do
  key <- pickKey generator
  if key `PartialKeySet.member` usedKeys
    then testHarness state
    else do
      (pkey, ()) <- spy key (action . fromKey generator)
      let observedCoverage = 0.1 -- TODO
      let score = (observedCoverage - coverageMean coverage)
            / sqrt (coverageVariance coverage)
      let generator' = updateGenerator score pkey generator
      let coverage' = updateCoverageExpectation observedCoverage coverage
      let usedKeys' = PartialKeySet.insert pkey usedKeys
      let state' = TestState (n + 1) generator' action usedKeys' coverage'
      when (shouldContinue state') $ testHarness state'

shouldContinue :: TestState -> Bool
shouldContinue (TestState n _ _ _ _) = n < 1000