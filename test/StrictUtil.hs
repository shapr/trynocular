module StrictUtil where

import Test.Hspec (expectationFailure)
import Test.QuickCheck (Arbitrary, stdArgs)
import Test.StrictCheck
  ( All,
    Produce,
    Spec,
    StrictCheck,
    equalToSpec,
    genViaProduce,
    shrinkViaArbitrary,
    strictCheckSpecExact,
    strictCheckWithResults,
    strictnessViaSized,
  )
import Test.StrictCheck.Curry (Args, Result)

strictCheck ::
  forall function.
  ( StrictCheck function,
    All Arbitrary (Args function),
    All Produce (Args function)
  ) =>
  Test.StrictCheck.Spec (Args function) (Result function) ->
  function ->
  IO ()
strictCheck spec function = do
  (maybeExample, _) <-
    strictCheckWithResults
      stdArgs
      shrinkViaArbitrary
      genViaProduce
      strictnessViaSized
      (equalToSpec spec)
      function
  case maybeExample of
    Just _ -> do
      -- Unfortunately, there's no easy way to print the result from this test.
      -- Instead, we'll rerun the test using strictCheckSpecExact, which prints
      -- a nice message but swallows the result.
      strictCheckSpecExact spec function
      expectationFailure $ "Strictness check failed."
    Nothing -> return ()
