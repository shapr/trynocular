module Trynocular.TestHarness where

import Control.Monad (when)
import Trace.Hpc.Reflect (examineTix)
import Trace.Hpc.Tix (Tix (..), TixModule (..))
import Trynocular.Generator (Generator, fromKey, pickKey)
import Trynocular.Key (PartialKey, spy)
import Trynocular.PartialKeySet (PartialKeySet)
import Trynocular.PartialKeySet qualified as PartialKeySet
import Trynocular.Quantiler
  ( CompleteQuantiler,
    Quantiler (..),
    emptyCompleteQuantiler,
  )

data TestState
  = forall params.
    TestState
      Int
      (Generator params)
      (params -> IO ())
      PartialKeySet
      (CompleteQuantiler Int)

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
    (emptyCompleteQuantiler 0.1)

updateGenerator ::
  -- | quantile for coverage observation
  Double ->
  -- | demanded portion of the key
  PartialKey ->
  -- | generator to update
  Generator a ->
  -- | new updated generator
  Generator a
updateGenerator _score _pkey gen = gen

testHarness :: TestState -> IO ()
testHarness state@(TestState n generator action usedKeys coverage) = do
  key <- pickKey generator
  if key `PartialKeySet.member` usedKeys
    then testHarness state
    else do
      (pkey, ((), observedCoverage)) <-
        spy key (observeCoverage . action . fromKey generator)
      let (coverage', coverageQuantile) = quantile coverage observedCoverage
      let generator' = updateGenerator coverageQuantile pkey generator
      let usedKeys' = PartialKeySet.insert pkey usedKeys
      let state' = TestState (n + 1) generator' action usedKeys' coverage'
      when (shouldContinue state') $ testHarness state'

shouldContinue :: TestState -> Bool
shouldContinue (TestState n _ _ _ _) = n < 1000

observeCoverage :: IO a -> IO (a, Int)
observeCoverage a = do
  preTix <- examineTix
  a' <- a
  afterTix <- examineTix
  pure (a', additionalTix preTix afterTix)

additionalTix :: Tix -> Tix -> Int
additionalTix old new = tixCount new - tixCount old

-- How many regions were executed at least once for this module?
tixModuleCount :: TixModule -> Int
tixModuleCount (TixModule _ _ _ regions) = sum $ 1 <$ filter (> 0) regions

-- How many regions were executed at least once for all these modules?
tixCount :: Tix -> Int
tixCount (Tix ms) = sum $ map tixModuleCount ms
