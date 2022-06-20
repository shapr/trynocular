# trynocular
library for lazy generators with observable demand
# the big idea
You can represent any value by a binary tree of the choices you made to reach that value.

# `Generator`, `GenKey`, and Strictness

The fundamental idea here is that a `rerunGenerator` constructs values from
`GenKey`s in a strictness-preserving way.  That is to say that partially
forcing the result of `rerunGenerator` will force the nodes of the `GenKey`
structure only when corresponding parts of the resulting value are forced.