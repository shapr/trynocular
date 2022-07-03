# trynocular
library for lazy generators with observable demand

# Why?

I want to mutate an input in some way that the new input runs more of the program being tested.

# the inspiration

Could I run gradient descent on random test inputs?

To be able to do that, I would need some way to generate an input and then find a "nearby" input to exercise parts of the program that haven't been evaluated yet.

Trynocular is an attempt to define "nearby" as well as a framework for detecting which parts of the input were used by the program under test.

# the big idea
You can represent any value by a binary tree of the choices you made to reach that value.

# `Generator`, `GenKey`, and Strictness

The fundamental idea here is that a `rerunGenerator` constructs values from
`GenKey`s in a strictness-preserving way.  That is to say that partially
forcing the result of `rerunGenerator` will force the nodes of the `GenKey`
structure only when corresponding parts of the resulting value are forced.

# Use plan

To test with a Generator, one would:

1. Initialize probabilities to 0.5 for each Choice node of the generator.
2. Initialize the coverage expectation to some initial value.
3. Loop as follows:
	1. Generate a random test case that differs from past test cases in some
	   portion that was forced by that test case.
	2. Run the test and see if it fails.
	3. Observe the demand on the key, and the percent of additional coverage
	   generated by the test.
	4. Compare the observed additional coverage to the expectation.
		a. If the observed additional coverage is less than the expectation,
		   update probabilities of each Choice node that was forced in the key
		   to decrease their probability.
		b. If the observed additional coverage is greater than the expectation,
		   update probabilities of each Choice node that was forced in the key
		   to increase their probability.
	5. Update the coverage expectation using the observed additional coverage,
	   using an exponentially weighted moving average.  That is, new coverage
	   expectation = alpha * old coverage expectation + (1 - alpha) * observed
	   new coverage.
	6. Repeat until some termination condition is satisfied.
