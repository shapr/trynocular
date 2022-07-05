# Problem overview

For a given test case, we have a `Generator` and a `PartialKey`.

- The `Generator` is a tree of decisions.  However, unlike a traditional
  decision tree, it has `Choice` nodes where only one branch is taken, and also
  `Both` nodes, where both branches are taken.

- The `PartialKey` is a record of the path through the tree.  At `Choice` nodes
  it records left or right.  At `Both` nodes, it splits into two paths: one for
  each subtree.

For example, a generator for the type `Either ((), Bool) ()` would look like
this:

```
                                    Choice
                                   /      \
                                  /        \
                                 /          \
                               Both       Trivial
                               /  \
                              /    \
                             /      \
                         Trivial   Choice
                                    /  \
                                   /    \
                                  /      \
                              Trivial  Trivial
```

These are the three possible total `Key`s:

```
                                    LeftF
                                   /
                                  /
                                 /
                               BothF
                               /  \
                              /    \
                             /      \
                         TrivialF   LeftF
                                    /
                                   /
                                  /
                             TrivialF
```

```
                                    LeftF
                                   /
                                  /
                                 /
                               BothF
                               /  \
                              /    \
                             /      \
                         TrivialF  RightF
                                       \
                                        \
                                         \
                                       TrivialF
```

```
                                    RightF
                                          \
                                           \
                                            \
                                          TrivialF
```

Consider this hypothetical key.  The node labeled "-" is the point at which the
key was no longer demanded, so this is essentially the leaf of the path.  There
is no point in updating nodes lower than that one.

```
                                  o            p_Left = 50%
                                 /
                                /
                               o               p_Left = 50%
                                \
                                 \
                                  o            p_Left = 50%
                                 /
                                /
                               -
```

Currently, this key is chosen with a 12.5% probability.  We will observe the
test and determine its *benefit*  (The current idea is to base the benefit on
code coverage from hpc, but this calculation makes no assumptions about the
nature of the benefit.)  If the benefit is greater than we have seen when
testing with other keys, then we want to increase the probability of generating
keys similar to this one.  If it is less than we have typically seen, though,
then we want to decrease the probability of generating keys like this one.

# Choosing a new target probability for a key

Bayes' Law tells us that for any events A and B, P(A) * P(B|A) = P(B) * P(A|B).
Consider the following events for a test with a random key:

* A = a test uses the observed `PartialKey`
* B = a test yields at least the observed benefit

We take P(B|A) to be 50% by assumption.  Although we observed the benefit and
know its exact value, it is right on the line and it makes sense to assign half
credit to preserve continuity.

P(A) is computed exactly as the product of the prior probabilities associated
with all choice nodes of the `Generator` that were forced by the test.

P(B|~A) is estimated by comparing the observed benefit with typical benefit.
The `Quantiler` class abstracts over various techniques for estimating this
probability.  It can be done with statistical methods (e.g., exponentially
weighted moving average) or by storing the entire set of past benefits.
Whatever the method for estimating P(B|~A), P(B) is then calculated easily:
P(B) = P(A) * P(B|A) + (1 - P(A)) * P(B|~A).

Applying Bayes' Law, we can now estimate P(A|B), the conditional probability of
using the observed key given that a test yields at least the observed benefit.
In a perfect world, we would adjust the `Generator` so that it generates the
observed `PartialKey` with this probability (and therefore other similar values
more or less often as well).  However, because the chosen value for P(B) was
only an estimate, it behooves us to be more careful, and simply adjust in the
direction of this target probability by some configurable step size.

For example, using the key above with a prior probability of 12.5%. suppose we
estimate P(B|~A) = 30%.  This was an above average example.  Then:

* P(B|A) = 50%
* P(A) = 0.5 * 0.5 * 0.5 = 12.5%
* P(B|~A) = 30%
* P(B) = 0.125 * 0.5 + 0.875 * 0.3 = 32.5%
* P(A|B) = 0.5 * 0.125 / 0.325 = 19.23%

As expected, the probability of this key was increased to reflect that it does
better than the previous average.  On the other hand, if we estimate P(B|~A) =
70%, making this a below average key, then:

* P(B|A) = 50%
* P(A) = 0.5 * 0.5 * 0.5 = 12.5%
* P(B|~A) = 70%
* P(B) = 0.125 * 0.5 + 0.875 * 0.7 = 67.5%
* P(A|B) = 0.5 * 0.125 / 0.675 = 9.26%

The below average key had its probability decreased.  Finally, suppose the
observed benefit is remarkably high, so P(B|~A) is only 1%.  Now:

* P(B|A) = 50%
* P(A) = 0.5 * 0.5 * 0.5 = 12.5%
* P(B|~A) = 1%
* P(B) = 0.125 * 0.5 + 0.875 * 0.01 = 7.125%
* P(A|B) = 0.5 * 0.125 / 0.07125 = 87.72%

Do we really want an 88% chance of choosing this specific key?  No!  In fact,
this would just result in choosing the same key over and over again, and the
test harness would skip these redundant tests.  The issue here is that when
P(B|~A) is extreme, it's more likely that the estimate of P(B|~A) is wrong than
that the chosen key is really that great.  This illustrates why we might choose
a smaller step size to move in the direction of the target probability without
stepping all the way there in a single iteration.

# How to adjust the target key probability

Recall the example key from above.

```
                                  o            p_Left = 50%
                                 /
                                /
                               o               p_Left = 50%
                                \
                                 \
                                  o            p_Left = 50%
                                 /
                                /
                               -
```

The probability of choosing this key is 12.5%.  Suppose we want to adjust that
probability to a new target, such as 20%.  We must accomplish this by adjusting
the `p_Left` value at each individual node of the tree.  There are, of course,
many ways that we could change these three probabilities so that they have the
desired product.  Which shall we choose?

We seek to make the desired adjustment by changing the probabilities of only
those `Choice` nodes in the generator that were reached by the given test.
(Notably, a `Choice` node is reached if and only if it appears in the generated
`PartialKey`.  A node that was reached to generate the total test input, but
whose choice was never observed by the test itself, should not be adjusted.)
The overall probability of the key is the product of the probabilities of making
the observed choice at all of those choice nodes.  Thus, we decompose the
probability into `p = p_1 * p_2 * ... * p_n`, where `p_i` is the probability
of making the choice at the `i`th `Choice` node.  We can only directly modify
the various `p_i` to have the desired cumulative effect on `p`.

We can reason from the extremal case to see how to do this.  If we wanted
`p = 1` (multiplying it by a factor of `k = 1 / p`), then clearly all `p_i` must
be changed to 1 as well.  Then `p_1` should be multiplied by `1 / p_1`, which is
`k ^ (log p_1 / log p)`, and `p_2` should be multiplied by `1 / p_2`, which is
`k ^ (log p_2 / log p)`, and so on.  Back in the general case, we can check that
these same exponents still work to multiply `p` by an arbitrary factor `k`, so
long as `p` is strictly between 0 and 1.

(If `p = 0`, then it's impossible that we observed this key at all, so we can
dispense with this case.  If `p = 1`, then we will not adjust the probability.
In practice, this means that the test never observed the result of a
non-trivial `Choice` node at all.  We treat `Choice` nodes with a probability
equal to 1 as trivial.  The approach described here ignores them, since the log
of their probability is 0.)

In the example above, the probability of the key is 12.5%.  Suppose we want to
adjust it to 25%, so `k = 2`.  Then we have:

* `p_1 = p_2 = p_3 = 0.5`
* `log p = -3`, while `log p_1 = log p_2 = log p_3 = -1`.
* All three `Choice` nodes have their selected direction multiplied by
  `2 ^ (1/3)`, the cube root of 2.
* The resulting combined probability is multiplied by 2 as a result.

On the other hand, suppose the three nodes had probabilities of `0.1`, `0.2`,
and `0.3`.  Then `p = 0.1 * 0.2 * 0.3 = 0.006`.  Now if we wanted to multiply
its probability by `k`, we would have:

* `log p ≈ -7.38`, `log p_1 ≈ -3.32`, `log p_2 ≈ -2.32`, and `log p_3 ≈ -1.74`
* `p_1` is multiplied by `k ^ 0.45`
* `p_2` is multiplied by `k ^ 0.31`
* `p_3` is multiplied by `k ^ 0.24`
* These exponents sum to 1, so the combined probability is multiplied by `k`.
