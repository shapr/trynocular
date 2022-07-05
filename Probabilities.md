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
*additional* coverage obtained by testing with this key.  If it is more than
we have seen when testing with other keys, then we want to increase the
probability of generating keys similar to this one, since it reaches lots of
previously untested code.  If it is less than we have typically seen, though,
then we want to decrease the probability of generating keys like this one.

# Choosing a new target probability for a key

Bayes' Law tells us that for any events A and B, P(A) * P(B|A) = P(B) * P(A|B).
Consider the following events for a test with a random key:

* A = a test uses the observed `PartialKey`
* B = a test yields at least the observed additional coverage

We take P(B|A) to be 50% by assumption.  Although we observed the coverage and
know its exact value, it's right on the line and it makes sense to assign half
credit to preserve continuity.

P(A) is computed exactly as the product of the prior probabilities associated
with all choice nodes of the `Generator` that the key passes through.

P(B|~A) is estimated by comparing the observed additional coverage with previous
observations.  Techniques vary here, anywhere from storing the entire set of
past observations to summarizing the distribution with an exponentially weighted
moving average.  Whatever the mechanism for estimating P(B|~A), P(B) is then
calculated easily: P(B) = P(A) * P(B|A) + (1 - P(A)) * P(B|~A).

Applying Bayes' Law, we can now estimate P(A|B), the conditional probability of
using the observed key given that a test yields at least the given coverage.
In a perfect world, we would adjust the `Generator` so that it generates the
observed `PartialKey` with this probability (and therefore other similar values
more often as well).  However, because the chosen value for P(B) was only an
estimate, it behooves us to be more careful, and simply adjust in the direction
of this target probability by some configurable step size.

For example, suppose we compute P(B|~A) from a z-score, and the z-score is 0.5.
Then:

* P(B|A) = 50%
* P(A) = 0.5 * 0.5 * 0.5 = 12.5%
* P(B|~A) = 30.85%
* P(B) = 0.125 * 0.5 + 0.875 * 0.3085 = 33.24%
* P(A|B) = 0.5 * 0.125 / 0.3324 = 18.80%

On the other hand, if the z-score is -0.5, then:

* P(B|A) = 50%
* P(A) = 0.5 * 0.5 * 0.5 = 12.5%
* P(B|~A) = 69.15%
* P(B) = 0.125 * 0.5 + 0.875 * 0.6915 = 66.76%
* P(A|B) = 0.5 * 0.125 / 0.6676 = 9.36%

Now suppose the z-score is 2.5, so this was a phenomenal example.  Now:

* P(B|A) = 50%
* P(A) = 0.5 * 0.5 * 0.5 = 12.5%
* P(B|~A) = 0.6210%
* P(B) = 0.125 * 0.5 + 0.875 * 0.006210 = 6.79%
* P(A|B) = 0.5 * 0.125 / 0.0679 = 92.05%

Do we really want a 92% chance of choosing this specific key?  No!  In fact,
this would just result in choosing the same key over and over again, and the
test harness would skip these redundant tests.  The issue here is that when
P(B|~A) is extreme, it's more likely that the estimate of P(B|~A) is wrong than
that the chosen key is that promising.  This illustrates why we might choose a
smaller step size to move in the direction of the target probability without
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

More generally, at any `Choice` or `Both` node of a `Generator`, there are two
probabilities that can be multiplied together to obtain the probability of
choosing any given key.  At a `Choice` node, those are the probability of
choosing the branch that matches the key, and the tail probability *after*
the choice.  At a `Both` node, these are the respective tail probabilities of
choosing the correct left and right halves of the key, and these probabilities
are multiplied because the choices are made independently.  The third relevant
node type is a `Trivial` node, in which case the correct key is generated with
100% probability.  Any node for which the partial key is not defined is also
treated as a `Trivial` node.

We can reason from the extremal case.  If we'd like to adjust the probability to
be 1, then clearly both of the constituent probabilities must both become 1 as
well.  So given an initial probability `p * q`, we want to multiply it by
`k = 1 / (p * q)`.  We must multiply `p` by `1 / p`, which is
`k ^ (log p / log (p * q))`, while we multiply `q` by `1 / q`, which is
`k ^ (log q / log (p * q))`.  We can check that these exponents for `k` work in
general, except for the case where `p` and `q` are both 100%.  We treat that
case as impossible.  (For example, we cannot adjust the probability of
`BothF TrivialF TrivialF`!)

In the example above, the probability of the key is 12.5%.  Suppose we want to
adjust it to 25%, so `k = 2`.  Then at the top node, we compute (using base 2
logarithms for easier math, but it doesn't matter in the end):

* `p = 0.5` and `q = 0.25`
* `p' = p * k ^ (log p / log (p * q)) = 0.5 * 2 ^ (1/3)`
* `q' = q * k ^ (log q / log (p * q)) = 0.25 * 2 ^ (2/3)`

We can update `p` immediately.  To adjust `q`, though, we must recursively
update the subtree below the `Choice` node.  Now `k = 2 ^ (2/3)`, and we compute
the following for the middle node.

* `p = 0.5` and `q = 0.5`
* `p' = p * k ^ (log p / log (p * q)) = 0.5 * 2 ^ (1/3)`
* `q' = q * k ^ (log q / log (p * q)) = 0.5 * 2 ^ (1/3)`

Again, we may update `p` directly in the node, but we must recursively update
the subtree below the `Choice` node.  Now `k = 2 ^ (1/3)`, and we compute the
following for the bottom node.

* `p = 0.5` and `q = 1`
* `p' = p * k ^ (log p / log (p * q)) = 0.5 * 2 ^ (1/3)`
* `q' = q * k ^ (log q / log (p * q)) = 1`

Effectively, in this case, we have multiplied all three probabilities in the key
by `2 ^ (1/3)`, which had the effect of doubling the probability of the key.
However, had the original probabilities not been equal, the adjustment would
have been allocated differently.