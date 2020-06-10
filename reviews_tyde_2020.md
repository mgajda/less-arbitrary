Review #1A
===========================================================================

Overall merit
-------------
1. Reject

Reviewer expertise
------------------
4. Expert

Paper summary
-------------
# Summary

The paper addresses the problem of inferring precise types for
semistructured data (JSON) from a black-box foreign source (e.g. a web
API). To do so it combines an algebraic approach (i.e. in terms of
mathematical structures, operations, and their laws) with techniques
from machine learning literature (i.e. reinforcement learning using
priors).

Type inference is framed as a version of a constraint unification
problem. On the PL side, "non-strict" type inference of an open
(rather than fixed) calculus of terms is facilitated by relaxing
standard subtype semilattice inequality operations to not demand
idempotence. On the ML side, techniques such as sampling and using
priors like "optionality" (minimization of sum types) steer the
algorithm towards "natural" types for the witnessed JSON example data.

# Evaluation

I appreciate trying to blend PL and ML in an appropriate application
area such as this, assigning types to data from an uncontrolled
source. Additionally, the overall structure of the approach that uses
lawful algebraic operations over a "typelike" domain, its "typing
relation", and framing type inference in terms of "typelike"
constraints seems like an interesting approach.

However, the primary issue with the paper is that in its current state
it reads more like a draft than a completed research paper. The
paper's structure seems jumbled and without clear direction, rather
than having a smoothly flowing narrative that introduces readers to
high-level concepts with examples before going into
details. Additionally, the paper reads more like a sparsely documented
library than a prosaic explanation of essential ideas. For example,
pages 5, 8, and 9 are almost entirely filled with code rather than
explanations, diagrams, figures, or example inputs and outputs.

An greater attempt should be made to paint an "overall picture" of
what happens in the various stages of the algorithm, along with
callouts to how each algorithmic component interacts with running
examples. To have space to do so, I recommend removing parts of the
algorithm that are already covered by representative samples, e.g. on
page 5 the code for `Int` or `String` is enough, both are not
needed. Further, I recommend abandoning the literate document style in
favor of a more standard judgemental presentation of the
algorithm. This has the benefit of capturing the language-independent
essence of the ideas, and more importantly is space efficient enough
to even include the complete algorithm in referenceable figures while
representative portions are highlighted in the main narrative.

Additionally, the paper should do a better job of giving some more
background on ML concepts like reinforcement learning and priors,
and exactly how they apply to this problem, to a PL audience.

In its current form, the paper is unable to effectively communicate
its ideas and needs more polish. It may or may not deserve publication
on the merits of its contributions, but at the moment it is not
presented well enough to tell, and it requires more comparisons to
related work.

# Related Work

The related work section is sparse and does not go into much
detail. For example, none of the extensive literature on languages for
typing XML (more general than JSON) is even mentioned (e.g. XDuce or
Comega). Additionally, comparisons of "non-strict" typing for
reinforcement learning, somewhere in-between static and dynamic,
should be made with gradual typing. I also expect inductive
programming research to be relevant here.

Comments for author
-------------------
I think there's promise in the work and hope you won't be discouraged, it just needs to go through more rounds of revisions and compared better with related work before it becomes pedagogically useful to the research community.

* 18: with "the" ubiquity
* 73: semantic subtyping literature completely characterizes union types
* 96: missing comma in dictionary
* 105: reader doesn't know what a "repr" should mean at this point
* 100 + 105: Value and HType used before described
* 111-165: strange and difficult-to-read mix of lists with examples and those without
* 239: mention `mappend` is join, not meet
* 252: malformed "quickcheck" reference
* 255-259: duplicate paragraph
* 261: needs a reference for "beyond set" in "permissive union types"
* 280: related to gradual typing
* 307: confusing usage of "strict" to describe Haskell
* 318: be more clear earlier that dropping idempotence is fundamental
* 327: malformed "forall"
* 333: we didn't see anything, give more thorough worked-through example of non-monotonic inference
* 410: "constraints" spelling
* 419: callout non-standard reversing of typing relation in `Types`
* 431: "om" spelling
* 444: confusing "X :: x" typing relation flipped again
* 448: aka "base types"
* 451: "values" spelling
* 649: "String" should be "Int"
* 677: "former" ?
* 1274: in "a" most


* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *


Review #1B
===========================================================================

Overall merit
-------------
3. Weak accept

Reviewer expertise
------------------
3. Knowledgeable

Paper summary
-------------
This paper describes an approach in Haskell to infer adequate union types from JSON examples.
The approach is learning-based, and always infer types that accept all the examples. A cost function is defined that allows to choose the "best" type corresponding to the input, making choices between seeing objects as records or maps for instance.

Comments for author
-------------------
The ideas presented here are interesting, and I believe original, at the very least in their type class representation.

The main weakness I see is that the paper is rather short on explanations, and at times cryptic. It contains also lots of code, which may make it a candidate for an implementation pearl (but I don't think this is the authors intent). Actually, one could argue that this paper itself is an experiment in learning from examples, in that I could eventually understand its goal after reading lots of slightly repetitive inference code.

One thing that left me particularly confused is this paper's use of the "principal type property".
My understanding is that a principal type is such that all valid types, and only them, can be derived from it. But here, it only means that the "principal type" is a valid type for all the examples given as input, with no clearly defined notion of subsumption. In particular, there are rules saying that an enumeration of more than 9 cases is converted to Any, which seems an overgeneralisation, contradicting the principality property.
 It may be the case that your definition indeed corresponds to  principal types in their usual meaning, but some definitions seem lacking (i.e., what are the well-formed types, and what is the subsumption relation).

Other comments:
All over the paper:
  Your use of footnotes is very unusual. This breaks the linearity of reading.

p1, end of introduction:
 "... the laws of soundness [...] are abandoned in practice of larger systems [17]"
 You seem to imply that GHC abandons soundness for practicality. I don't think this reflects correctly Simon's intention.

p3, right column:
  Discussions on "closed to information" and "abolish the semilattice requirement" seem to be repeated twice. There is something wrong here.

Footnote 11: "Please refer to the counting example below"
  In general, it is bad idea to refer people to something yet to come. More so when you do it in footnotes.

p5, right column:
  "infer _ = SCAny
   infer value = SCEnum $ Set.singleton value"
  How can "infer value" ever be reached through pattern-matching?
  "mappend = (<>)"
  Why do you need to define mappend here and not for IntConstraint?

p9, line 987:
  What is a "levitated semilattice" ?

p10, line 1075:
  "Since ..., we need to sum all options ..."
  Why is it so? I do not see the reasoning behind this implication.


* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *


Review #1C
===========================================================================

Overall merit
-------------
1. Reject

Reviewer expertise
------------------
3. Knowledgeable

Paper summary
-------------
This paper studies the problem of trying to recover type information from "unstructured data" given in the form of a set of JSON values. It  motivates the problem with some examples, and then develops a Haskell library that implements type inference.

Comments for author
-------------------
While the problem is a very interesting one, in its present state the paper is unsuitable for TyDE for two reasons.

First, the author is unaware of the large literature on the topic of inferring types from unstructured data. A recent example is this paper: https://openproceedings.org/2017/conf/edbt/paper-62.pdf
which is on exactly the same topic (inferring types from JSON) and has pointers to more classical literature often in the DB community, that looks at XML instead.

Second, the writing needs to be significantly improved.
At a high level, before the paper describes the actual algorithm, you must tell me what the _target_ is -- that is _what_ exactly are we trying to infer? That is, what is the language of types?
What are the properties desiredof the inference algorithm and so on. Otherwise, one can imagine all kinds of different solutions and its not really clear what is desirable.

At a lower level, as it stands right now, the bulk of the paper reads like comments around Haskell code. In many cases, there are little chunks of code that have been commented out and which really should be removed entirely.

Nevertheless, there are some nice ideas here -- I really like the idea of using a monoid to represent the types, and to have the <> operator "join" the types learned from different samples.
I strongly encourage the author to work on the paper, starting by clearly focusing on the *desired outcome* of the type inference, and I think at that point this will be a very nice contribution!

