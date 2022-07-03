For a given test case, we have a `Generator` and a `PartialKey`.

- The `Generator` is a tree of decisions.  However, unlike a traditional decision tree, it has `Choice` nodes where only one branch is taken, and also `Both` nodes, where both branches are taken.

- The `PartialKey` is a record of the path through the tree.  At `Choice` nodes it records left or right.  At `Both` nodes, it splits into two paths: one for each subtree.

Example:

The generator is:
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

These are the three possible keys:

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
                                          Trivial
```

What we know is the value we got from a test case, given the choice of key. What we want to know is the right key probabilities to optimize the expected value of future generated keys.  This is vaguely Bayesian in nature.  How do we make it more like a Bayesian probability question?

P(B|A) = P(A|B) * P(B) / P(A)

Turning the score into a probability.

If we know the mean and variance of a distribution for an arbitrary key.  And we know that this particular key scored x stddevs above the mean.

We can compute P of scoring at most `score` stddevs above the mean.  If this probability is > 50%, we want the distribution to be less like this key.  If less than 50%, we want the distribution to be more like this key.

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
                              / \
                            ... ...
```

A = we chose this key
B = we scored this well

P(we scored this well | we chose this key) =
    P(we chose this key | we scored this well)
    * P(we scored this well)
    / P(we chose this key)

To an approximation (assuming some jitter in measurement):

P(we scored at least this well | we chose this key) = 50%
P(we scored at least this well) is computed from score.
P(we chose this key) = product of the probabilities of all the choice nodes in the path.

We can compute P(we chose this key | we scored at least this well)

P(we chose this key | we scored this well) =
    P(we scored this well | we chose this key)
    * P(we chose this key)
    / P(we scored this well) =


In the example, if score = 0.5, then:

P(we scored this well | we chose this key) = 50%
P(we chose this key) = 0.5 * 0.5 * 0.5 = 12.5%
P(we scored this well) = 30.8538%
P(we chose this key | we scored this well) = 0.5 * 0.125 / 0.308538 = 20.26%

On the other hand, if score = -0.5, then:

P(we scored this well | we chose this key) = 50%
P(we chose this key) = 0.5 * 0.5 * 0.5 = 12.5%
P(we scored this well) = 69.1462%
P(we chose this key | we scored this well) = 0.5 * 0.125 / 0.691462 = 9.04%

Once we know the new and old probability of choosing this key, we can compute their ratio, and then count the number of choices made in the key.  Take that root of the ratio, and multiply the probability at each choice node by that root.  For example, in the score = -0.5 case, we want to update P(key) from 12.5% to 9.04%, which is a factor of 0.7232.  There are three choices in the key, so the cube root of 0.7232 is 0.8976.  We should multiply each choice node by that factor.

Specifically: when we went left, we should multiply p_Left by 0.8976.  When we went right, we should multiply p_Right by 0.8976.  But since the code only stores p_Left, then p_Left,new = 1 - p_Right,new = 1 - 0.8976 * p_Right,old =
1 - 0.8976 * (1 - p_Left,old).

Revisiting the problem case:

Suppose score = 2.5.  We did really well!  Yay!  Now:

P(we scored this well | we chose this key) = 50%
P(we chose this key) = 0.5 * 0.5 * 0.5 = 12.5%
P(we scored this well) = 0.6210%
P(we chose this key | we scored this well) = 0.5 * 0.125 / 0.006210 = 1006.44%

Do we really want a 1000% chance of choosing this key?  NO!  In fact, if we had a 100% chance of choosing this key, then our test harness would stall forever because it never chooses a unique key.

The issue here is that P(we scored this well) is wrong.  In fact, it MUST be at least half of P(we chose this key).  But even so, we never want to choose a probability of 100% because we're not sure of P(we scored this well), so 100% is too extreme.  So maybe we have a weighting factor that discounts the new observation and just moves partially in that direction.

Other possibility: When we choose a key that has already been used, decrease the probabilities for that key by a slight amount to prevent spinning forever just looking for a unique key.