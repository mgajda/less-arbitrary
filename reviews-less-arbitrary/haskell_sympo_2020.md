---
title: Haskell Symposium 2020 reviews
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


* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *


Review #11B
===========================================================================

Overall merit
-------------
1. Reject

Reviewer expertise
------------------
3. Knowledgeable

Comments for author
-------------------
There's something interesting in this paper but it's not ready for publication.  It's hard to understand, doesn't present the goals or achievements clearly, and does not explain why using a state monad (rather than an environment monad (a la 'sized' stuff) is so much better.

Reject, with enthusiasm!  Please explain all this better, and you'll get a vastly larger readership

* Be explicit about what "better properties" means.  I think you have in mind (a) short, or at least predictable, runtimes for QuickCheck, (b) good coverage (whatever that means -- it's tricky).  Your paper badly needs an evaluation section that demonstrates, with measurements, the improvement achieved by your approach.  You claim that something is linear rather than exponential (which sounds good) but I'm not sure what that something is.  (Obviously, any recursive data structure data structure has an infinite number of instances, so it can't be that.)

* Section 5 starts a long sequence that assumes familiarity with Generics.  I *strongly* suggest you instead begin by giving manually-written LessArbitrary instances for lists, trees, syntax trees, without using Generics.  That will help show some general patterns, which you can, later, embody in a general scheme via Generics.  But at present Section 5 is nigh incomprehensible to me.  I was utterly lost, which is sad because the main payload of the paper is buried in there.

* You badly need a related-work section. The task of generating good test cases has been extremely widely studied, and there must be a lot of related work. (I'm no expert, but you should be.)  Please give us a guide tothe best related work is.


Smaller comments

* Generally: really strange font for program text... very widely space letters.  (Ugly.)  It's particularly terrible on line 67 where I think tree ' is meant to be tree', an identifier with a tick-mark as its last character.

* Line 45. But *why* will it fail to terminate?  Give us the insight please -- it is central to your paper.

* Line 38. Really hard to understand this without the class decl for Arbitrary, type signatures for oneOf, sized, etc.  Please give these in a Figure.  Or you can say "don't bother to read this paper unless you are thoroughly familar with Quickcheck and its types", but I don't think you want to say that.

* Line 47.  What is `lessArbitrary`??   It's not in QuickCheck, so far as I know http://hackage.haskell.org/package/QuickCheck-2.14/docs/Test-QuickCheck.html

* Line 82.  I didn't really understand your explanation of what goes wrong with the manual method.

* Line 95. I did not understand the method you propose here; not why it would "certainly fail to terminate"; nor why it would have "poor coverage".   At a bare minimum you need an example or two.

* Line 113 "we see some exponential function".  What does "see" mean?  I'm really lost.

* Line 132.  `(-c +)` is a terribly obscure idiom.  Use `(\x. x-c)`.

* Line 152 what is withCost?  In any case, you are just spinning out these definitions without justifying them.  Here you want to say that to make an arbitrary value you want to run your stateful CostGen computations with a fixed starting amount of fuel, gotten from QuickCheck.

* Line 162. I have no idea what `instance LessArbitrary where` means.  Looks ill-formed to me.

Language: there's a terrific shortage of the definite article.  Eg. line 61 "Indeed QuickCheck manual..." instead of "Indeed, the QuickCheck manual".  Ditto line 30, 32, line 45 (twice), 62, 63.  That's just in the first half of the first section.  It's not incomprehensible but it is distracting.  Getting a native English speaker to help at a later stage would be a good investment.


* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *


Review #11C
===========================================================================

Overall merit
-------------
1. Reject

Reviewer expertise
------------------
4. Expert

Paper summary
-------------
This paper proposes a way of automatically generating arbitrary instances
of recursive data types, to be used for property based testing (e.g., with
QuickCheck).  In comparison to standard practice in generating such
arbitrary instances, or using existing automatic techniques, the authors
emphasize on guaranteeing:

- that each instance can be generated in linear time (w.r.t. the test size),
- that the instance's size will be linear (w.r.t. the test size), i.e.,
  applying a linear operation on it will take linear time.

Comments for author
-------------------
The authors suggest two very simple ideas in this paper:

1. Replace `Gen` with an appropriate composition of a state monad that
   keeps track of the size of the instance that is generated.  In this way,
   no explicit `resize` operators will be needed; both termination and
   linear size of the instance can be guaranteed.
2. Arbitrary instance generation can be done automatic for all types that
   are instances of class `Generic`.

The first idea does not tell us something really new.  This is what
programmers do in practice, when they have to generate arbitrary terms
of given size: they use the target size as some kind of budget; they
plan and keep track of the subterm's sizes so that their total size does
not exceed the budget.  The authors suggest that this can be done
automatically with a state monad and that is true.  However, doing it
right is not as easy as the authors suggest.

Suppose we have a ternary tree data type:
```Haskell
data Tree a = Empty | Node a (Tree a) (Tree a) (Tree a)
```

A hand-written instance of `Arbitrary`, keeping track of tree size,
could look like this:
```Haskell
instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = sized f
    where f n | n > 0 = do
            -- try to make: sl + sm + sr = n-1
            xl <- choose (0.0, 1.0)
            xm <- choose (0.0, 1.0)
            xr <- choose (0.0, 1.0)
            let s = xl + xm + xr :: Double
            let sl = round (xl / s * fromIntegral (n-1))
            let sm = round (xm / s * fromIntegral (n-1))
            let sr = round (xr / s * fromIntegral (n-1))
            -- then generate the node
            Node <$> arbitrary <*> f sl <*> f sm <*> f sr
          f _ = return Empty
  shrink (Node x l m r) =
    Empty : l : m : r : [ Node x' l' m' r'
                        | (x', l', m', r') <- shrink (x, l, m, r) ]
```
It's far from trivial but, most importantly, it requires that we
**plan ahead** and generate subtrees of appropriate sizes, in order
to reach the budget `n`.  With this instance of `Arbitrary`, we run
the following program that tests a dummy property and classifies the
generated trees in four categories: 0 (empty), 1 (the left subtree
is the largest), 2 (the middle subtree is the largest), and 3 (the
right subtree is the largest):
```Haskell
classof Empty = 0
classof (Node _ l m r) | sl == largest = 1
                       | sm == largest = 2
                       | sr == largest = 3
  where sl = size l
        sm = size m
        sr = size r
        largest = maximum [sl, sm, sr]
        size Empty = 0
        size (Node _ l m r) = 1 + size l + size m + size r

prop_dummy :: Tree Int -> Property
prop_dummy t = collect (classof t) $ True

main = quickCheckWith stdArgs { maxSuccess = 1000, maxSize = 1000 } prop_dummy
```
[The program](https://pastebin.com/ZUNu8kEB) prints the following:
```
$ ./qc-std-terntree
+++ OK, passed 1000 tests:
34.4% 1
33.1% 2
32.4% 3
 0.1% 0
```
generating very few empty trees and many non-empty trees in which the
largest subtree is picked with **equal probability**.

On the other hand, replacing the hand-written instance with:
```Haskell
instance LessArbitrary a => LessArbitrary (Tree a) where

instance LessArbitrary a => Arbitrary (Tree a) where
  arbitrary = fasterArbitrary
```
[the program](https://pastebin.com/EJ9EHURk) now prints:
```
$ ./ls-arb-terntree
+++ OK, passed 1000 tests:
49.6% 0
27.8% 1
12.4% 2
10.2% 3
```
The first thing to notice is that too many empty trees are generated.
But, most importantly, if we omit the empty trees and normalize the
probabilities of the other three categories, the three remaining
probabilities are unbalanced: the left subtree of a non-empty tree
is the largest with a probability of 55%, then the middle subtree
with a probability of 25%, then the right subtree with a probability
of 20%.  This should be expected if the generation of subterms occurs
left-to-right and is **not planned**: the size of the left subtree
is a random variable ranging over the entire "budget" therefore having
an expected value of `n/2`, then the size of the middle subtree is a
random variable ranging over the remaining "budget", therefore having
an expected value of `n/4`, and so is the size of the right subtree.
With 50% probability, the left subtree is expected to be the largest
and this is verified by the experiment above.

Overall, unless the authors suggest some clever way of **planning
ahead** for fairly determining the sizes of generated subterms,
the first subterms to be generated will always be the largest.
Try building a list of lists with `LessArbitrary`, for a custom
list data type and you'll definitely see that in large generated
first-level lists, the last second-level lists are always empty.

The second idea is again nothing really new.  I would expect a
comparison with existing automatic generators, e.g. (the first
two appearing in a quick Google search):

- `Test.QuickCheck.Arbitrary.Generic` (package: `generic-arbitrary`)
- `Data.Derive` (package: `derive`)

However, the authors seem to discard all related work.  The
bibliographic references are very few, some totally irrelevant,
and most of them are not even cited in the text.  Had the authors
compared their work with approaches like the ones above, they would
have argued that automatically derived instances for data types
such as the ternary tree above lead to generators that do not terminate,
because they lack the first idea.  This would be a fair point for the
authors.  But, I would like to see a comparison of the machinery used
by these approaches and, most importantly, benchmarks.  The authors'
approach, based on `Generic`, is dynamic.  What could be gained in
terms of performance by using a static (e.g., TemplateHaskell) approach?

One other drawback of `LessArbitrary` is that `shrink` does not seem to
be working.  The [following program](https://pastebin.com/dn8rMJjc) tests
among other things a wrong "merge tree" implementation.  With a standard
`instance Arbitrary` like the one shown above, it shows:
```
$ time ./qc-std-tree
Testing wrong: merged has no smaller size
*** Failed! Falsified (after 4 tests and 4 shrinks):
(*)
Node 0 []
Node 0 [Node 0 []]
Testing merge: merged has no smaller size
+++ OK, passed 10000 tests.
Testing dfn: same structure
+++ OK, passed 10000 tests.
Testing dfn: right order
+++ OK, passed 10000 tests.
Testing bfn: same structure
+++ OK, passed 10000 tests.
Testing bfn: right order
+++ OK, passed 10000 tests.
./qc-std-tree  8,92s user 0,44s system 97% cpu 9,562 total
```
On the other hand, with `LessArbitrary`, the
[same program](https://pastebin.com/buLtxzHU) fails to shrink the
counterexample.
Furthermore, it takes significantly longer to complete the test.
```
$ time ./ls-arb-tree
Testing wrong: merged has no smaller size
*** Failed! Falsified (after 3 tests):
(*)
Node 1 [Node 2 [],Node 1 [],Node (-2) [],Node (-2) [],Node 1 [],Node (-1) [],Node 2 [],Node (-1) [],Node (-1) [],Node 1 [],Node (-2) [],Node 1 [],Node 0 [],Node (-1) [],Node 0 [],Node 2 [],Node (-2) [],Node 1 [],Node 2 [],Node 0 [],Node 2 [],Node (-2) [],Node 1 [],Node 1 [],Node 0 [],Node 2 [],Node (-1) [],Node 0 [],Node (-2) [],Node 1 [],Node 1 [],Node 1 [],Node (-1) [],Node 1 [],Node (-1) [],Node 2 [],Node (-1) [],Node (-2) [],Node (-1) [],Node 0 [],Node (-1) [],Node 0 [],Node (-2) [],Node (-2) [],Node (-2) [],Node 1 [],Node (-1) [],Node (-1) [],Node 0 [],Node (-2) [],Node 0 [],Node 2 [],Node (-2) [],Node 2 [],Node (-2) [],Node 1 [],Node 1 [],Node 0 [],Node (-2) [],Node 1 [],Node 0 [],Node 0 [],Node 1 [],Node (-1) [],Node (-2) [],Node (-1) []]
Node 1 [Node 2 [],Node 2 [],Node (-1) [],Node 2 [],Node 1 [],Node 2 [],Node 1 [],Node 1 [],Node (-1) [],Node 0 [],Node 0 [],Node 0 [],Node 2 [],Node 2 [],Node (-2) [],Node (-2) []]
Testing merge: merged has no smaller size
+++ OK, passed 10000 tests.
Testing dfn: same structure
+++ OK, passed 10000 tests.
Testing dfn: right order
+++ OK, passed 10000 tests.
Testing bfn: same structure
+++ OK, passed 10000 tests.
Testing bfn: right order
+++ OK, passed 10000 tests.
./ls-arb-tree  12,54s user 0,55s system 96% cpu 13,534 total
```

On top of all this, the paper is not well written.  The structure is weak,
there's a lot of code and relatively few explanations.  The introduction
(§1) repeats the abstract almost verbatim.

Unfortunately, I believe that this paper cannot be published in
this form.  If the authors decide to resubmit this work, I suggest
that they try to address the issues I mentioned above.


* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *


Review #11D
===========================================================================

Overall merit
-------------
1. Reject

Reviewer expertise
------------------
1. No familiarity

Paper summary
-------------
Not applicable

Comments for author
-------------------
I'm not one of the original reviewers assigned to this paper and I did not read the paper in detail, so please ignore the scores. I just wanted to point out that you may find some past work I did relevant to your work on generic generation of data. The approach I took seems quite different to yours, but perhaps it still has some insights that could be useful to you.

José Pedro Magalhães. Generic Generation of Constrained Random Data. 71st IFIP 2.1 Working Group meeting, Zeegse, the Netherlands, 25/03/2014.
http://dreixel.net/research/pdf/ggcrd_pres_ifip14.pdf

José Pedro Magalhães and Hendrik Vincent Koops. Functional Generation of Harmony and Melody. Second ACM SIGPLAN International Workshop on Functional Art, Music, Modelling and Design (FARM'14), 2014. http://dreixel.net/research/pdf/fghm.pdf (essentially using the idea presented in the slides above, briefly mentioned in Section 4.3)
