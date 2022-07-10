module Trynocular.TestHarness where

import Control.Monad (when)
import Trace.Hpc.Reflect (examineTix)
import Trace.Hpc.Tix (Tix (..), TixModule (..))
import Trynocular.Generator
  ( Generator,
    adjustProbability,
    fromKey,
    keyProbability,
    pickKey,
  )
import Trynocular.Key (PartialKey, spy)
import Trynocular.Standardizer
  ( NormalStandardizer,
    Standardizer (..),
    normalStandardizer,
  )

data TestState
  = forall params.
    Show params =>
    TestState
      Int
      (Generator params)
      (params -> IO ())
      NormalStandardizer

initialTestState ::
  forall params.
  Show params =>
  Generator params ->
  (params -> IO ()) ->
  TestState
initialTestState generator action =
  TestState
    0
    generator
    action
    (normalStandardizer 5 5 0.1)

updateGenerator ::
  -- | quantile for the benefit
  Double ->
  -- | demanded portion of the key
  PartialKey ->
  -- | generator to update
  Generator a ->
  -- | new updated generator
  Generator a
updateGenerator pct pkey gen = adjustProbability pkey targetKeyProb gen
  where
    priorKeyProb = keyProbability gen pkey
    priorBenefitProb =
      priorKeyProb * 0.5 + (1 - priorKeyProb) * (1 - pct)
    idealKeyProb = 0.5 * priorKeyProb / priorBenefitProb
    targetKeyProb = (priorKeyProb + idealKeyProb) / 2

testHarness :: TestState -> IO ()
testHarness (TestState n generator action coverage) = do
  key <- pickKey generator
  print $ fromKey generator key
  (pkey, ((), observedCoverage)) <-
    spy key (observeCoverage . action . fromKey generator)
  putStrLn $ "   " <> show observedCoverage
  let (coverage', coveragePct) = percentile coverage $ fromIntegral observedCoverage
  let generator' = updateGenerator coveragePct pkey generator
  let state' = TestState (n + 1) generator' action coverage'
  if (shouldContinue state')
    then testHarness state'
    else do
      putStrLn $ "Ran this many test cases: " <> show (n + 1)

-- | at the 95 percentile, we should expect at least 1 additional coverage
shouldContinue :: TestState -> Bool
shouldContinue (TestState _ _ _ standardizer) = maybe False (> threshold) (fromPercentile standardizer 0.99) -- percentile where we expect more coverage
  where
    -- number of additional measured regions we expect to see covered after our new test
    threshold = 1

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

smartCheck :: Show params => Generator params -> (params -> IO ()) -> IO ()
smartCheck generator action = testHarness (initialTestState generator action)
