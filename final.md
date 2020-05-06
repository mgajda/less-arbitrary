# Choosing representation

## Heuristics for better types

The final touch would be to postprocess assigned type
before generating it, in order to make it more resilient
to common uncertainties.

Note that these assumptions my sidestep
our validity criterion from the initial part
of the paper, however they proved to work
well in practice.

### Array type with no element observations

If we have no observations of array type,
it can be inconvenient to disallow array to
contain any value at all.
Thus we make a non-monotonic step of
converting the `mempty` in the final `Typelike` to representation
allowing any `Value` there on the input.

That is because, our program must not have any assumptions
about these values, but at the same it should be able to
output them for debugging purposes.

## Overall processing scheme

```{ .dot width=48% height=13% #fig:dataflow }
digraph {
  margin=0;
  pad=0;
  node [margin="0.05,0",pad="0,0"];
  //bgcolor=gray;
  node [shape=box,color=white];
  rankdir=LR;
  subgraph g1 {
    "JSON Value" [label="JSON\nvalue"];
    "Typelike" [label="Infer"];
    rank=same;
  }
  subgraph g2 {
    "Choose representation" [label="Choose\nrepresentation"];
    "Match similar" [label="Unify\nsimilar"];
    rank=same;
  }
  subgraph g3 {
    "Breakdown" [label="Break\ndown\ninto\ndeclarations"]
    "Haskell code" [label="Haskell\ncode"];
    rank=same;
  }
  "JSON Value" -> "Typelike" -> "Match similar"
               -> "Choose representation"
               -> "Breakdown"
               -> "Haskell code";
  "JSON Value" -> "Choose representation" [style=invis];
  "Haskell code" -> "Match similar" [style=invis];
}
```

## Simplification by finding unification candidates

In most JSON documents we found that
the same object was described in different
parts of the sample datastructures.
Because of that, we compare sets of labels
assigned to all objects, and propose
to unify those that have more than 60% identical labels.

For transparency, candidates found are logged for the user,
and the user can also indicate them explicitly instead
of relying on automation.

We found that this greatly decreases the complexity of types,
and makes output less redundant.

# Future work

## Scaling to type environments

For now we have only discussed typing
of treelike values. However, it is natural
to scale this approach to multiple types in
API, where different types are referred to by
name, and possibly contain each other.

To address this situation, we show
that environment of typelikes is also `Typelike`,
and constraint unification can be  extended the same
way.

## Generic derivation of Typelike

Note that `Typelike` instances for non-simple types
usually follow one of two patterns:
1. For typing terms that have a finite sum
  of disjoint constructors,
  we bin this information by constructor
  during the `infer`ence
2. for typing terms that have two alternative
  representations we apply
  we `infer` all constraints separately,
  by applying the `infer`ence to the same term

In both cases derivation of `Monoid`,
and `Typelike` instances is the same.

That allows us to use GHC `Generic`s[@generics,@generic-monoid]
to define standard implementations
for most of the boilerplate code!

That means that we only will have to manually define:
* new constraint types,
* inference from constructors (case 1)
and entirety of handling alternative constraints
is implemented, until we choose representations.

## Conclusion

We derive types that are valid with respect
to specification, and thus give the best information
from the input.

We define type inference as representation learning,
and type system engineering
as a meta-learning problem, where our
our **priors about
data structure induce typing rules**.

We also make a mathematical formulation of
**union type discipline** as manipulation
of bounded join-semilattices with neutral element,
that represent knowledge given about the data
structure.

We also propose a union type system engineering
methodology, justifying it by theoretical criteria,
and showing that it consistently explains
our decisions in practice.

We hope that this kind of _formally justified type
system engineering_ will be more ubiquitous
in practice, replacing _ad-hoc_ approaches
in the future.

This paves the way towards formal
construction and derivation of type systems
from a specification of value domains
and design constraints.

# Bibliography {.unnumbered}

::::: {#refs}

:::::
