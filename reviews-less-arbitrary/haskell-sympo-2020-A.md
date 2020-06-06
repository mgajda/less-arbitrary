---
title: "Review #11A"
header-includes: |
  \DeclareUnicodeCharacter{2212}{--}
---

Review #11A
===========================================================================
* Updated: 2 Jun 2020 3:42:27am AoE

Overall merit
-------------
1. Reject

Reviewer expertise
------------------
2. Some familiarity

Paper summary
-------------
The generic instances of QuickCheck's `Arbitrary` class behave poorly when
applied to recursive data types, leading to generators that produce large test
cases and might even fail to terminate.  The QuickCheck manual suggests that
this issue can be fixed by leveraging a _size parameter_ that controls test-case
generation: when generating recursive arguments for a constructor, the size
parameter should be scaled down by the average branching factor of the data
type. Unfortunately, this method needs to be applied manually to each data type,
leaving room for bugs. Moreover, it still might produce very large test cases
(albeit generation is at least guaranteed to terminate).

The present paper proposes to fix this issue by extending the `Arbitrary` class
with a _cost parameter_.  The cost decrements every time we add a constructor to
the generated element.  This guarantees that the number of non-recursive calls
to `arbitrary` is linear in the cost.  The paper describes how to write generic
instances for this modified class, simplifying the task of writing efficient
test suites.

Comments for author
-------------------
I was not aware of the problem with QuickCheck's generic `Arbitrary`
instances. While the proposed fix seems simple and effective, I found the
presentation unsatisfactory in important aspects:

- The biggest issue was with the problem statement: you need to explain *why*
  the default strategies and the mitigation strategy proposed by QuickCheck may
  fail to terminate or lead to large test instances. Some math would be useful
  here; the discussion of §3 is not detailed enough. The abstract claims that
  you "prove connection between deeply recursive data structures, and epidemic
  growth rate [l. 11]", but there is no such analysis in the paper.

- There is no formal analysis of the expected running time of the generation
  strategy, only a claim that it runs in linear time. As I said in the summary,
  I believe you mean that the generation performs a linear number of calls to
  `arbitrary` for base types, but I would like to see that spelled out
  explicitly.

- There is no empirical evaluation of the proposed generation strategy.  One or
  two case studies would do (e.g. with variable-branching `Tree` type of the
  introduction).

- The paper spends too much time introducing GHC generics and discussing the
  implementation of the generic arbitrary instances.  For this audience, I
  imagine it would be enough to present a manual implementation of
  `LessArbitrary` for some type (e.g. `Tree`), and then sketch how you would
  generalize this.  Trimming this discussion would leave plenty of room to
  address the previous issues.

There were also some English issues; e.g. "we want to have fully generic
implementation" [l. 304] instead of "we want to have a fully generic
implementation". Revising those would make the text flow better.

## Minor

Put spaces between a reference and the preceding text; e.g `the awesome
library~\cite{awesome}` rather than `the awesome library\cite{awesome}`.

> Assuming we run QuickCheck with any size parameter greater than 1, it will
> fail to terminate! [l. 43]
>
> ...
>
> We compute a number of recursive references for each constructor. Then we take
> an average number of references among all the constructors. If it is greater
> than 1, any nonlazy property test will certainly fail to terminate. [l. 93]

I imagine you mean that the generator _can_ fail to terminate with non-zero
probability; after all, the generator could simply choose to generate a `Leaf`
from the beginning.

> `\size do` [l. 50]

Syntax error.

> Above example uses division of size by maximum branching factor ..., whereas
> dividing by average branching factor of ~2... [l. 73]

This is confusing, because the example divides the size exactly by 2. Do you
mean that the average branching factor is a bit smaller than 2, and that this
difference would lead to the size issues?

> would we not expect that tests terminate within linear time of this parameter?
> ... Currently for any recursive structure like Tree a, we see some exponential
> function. [l. 104]

This is important to understand the preceding discussion. It should have been
mentioned earlier.

> we hope that the the generic implementation will take over. [l. 163]

What do you mean? That is left as future work, or that this will be eventually
covered in the paper?

> There are more short cuts to consider: * U1 is the unit type (no fields) *
> Rec0 is another type in the field [l. 226]

Are the `*` item bullets? If so, you should display them as an itemized or
enumerated list?

> For the Monoid the implementation would be trivial, since we can always use
> mempty and assume it is cheap [l. 293]

This detour through `Monoid` is unnecessary.  I suggest removing it and going
straight into §5.2.1.

> we need to find the least costly constructor each time. [l. 309]

"Least costly" meaning with the fewest arguments? If so, it would be better to
say it explicitly.

> minimum cost of the in each branch of the type representation [l. 327]

Broken sentence.

> so we can choose the cheapest^[We could add instances for [l. 342]

Broken.

> <<flat−types>> [l. 350]

?

> We start with base cases GLessArbitrary for types with the same representation
> as unit type has only one result [l. 402]

The "has only one result" part is dangling.
