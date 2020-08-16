SUBMISSION: 15
TITLE: Towards the more perfect union type


----------------------- REVIEW 1 ---------------------
SUBMISSION: 15
TITLE: Towards the more perfect union type
AUTHORS: Michal Gajda

----------- Overall evaluation -----------
SCORE: 0 (borderline paper)
----- TEXT:
The author presents an approach to learn Haskell types from a couple of JSON structured inputs. The approach is interesting.

However, the paper is hard to read. The English requires a considerable improvement. Often, words are missing (in particular arcticles). Also some sentences are missing a verb or they are badly structured. I recommend that the author asks a native speaker to correct his English.
Alternatively, the author could use a tool such as Grammarly to check the grammar.

In addition, some parts need additional explanations; in particular the motivating examples in section 2.

For a workshop such as WFLP, the paper can be accepted. For the post-workshop proceedings, the presentation needs to be improved considerably.



----------------------- REVIEW 2 ---------------------
SUBMISSION: 15
TITLE: Towards the more perfect union type
AUTHORS: Michal Gajda

----------- Overall evaluation -----------
SCORE: -3 (strong reject)
----- TEXT:
I already had this paper for reviewing when it was submitted
to LOPSTR 2020. Therefore, I was interested whether the author
improved it since, in principle, it could be an interesting
apporach to type inference with union types, which
is relevant to add gradual typing to dynamically-typed languages.

Unfortunately, after looking at this submission,
I must realize that the author spent almost no effort
to improve it. As I already noticed in my first review,
the style of writing which makes the paper quite unreadable.
It seems that the author has many ideas in mind but is not
able to structure them into a written document.
He switches between concrete examples and more
abstract concepts without stating a clear relation
between them. I already mentioned some problems
in my first review. Some of them where easy to fix
but this was not done by the author. Therefore,
I do not expect an interesting talk from this paper.

Here are again some minor comments:

L. 24/25: A verb is missing in this sentence.

L. 79: Explain meaning of ":|:"

L. 86: What is "Haskell representation inference"?

L. 95: What is "Haskell encoding"?

L. 99: Why is this assumption correct?

L. 112/113: What is the type "Scientific"?

L. 190: Which figure? Make reference to Fig. 1. Sentence ended with ":".

L. 191: "The last law...": To which sequence of laws you refer?

L. 217/218: I guess this inference is possible only if you
require a fixed order on record labels. However,
{"a": X, "b": Y} and {"b": Y, "a": X} denotes the same record.

L. 300: It should be noted that ...

L. 355/356: This decision looks quite ad hoc. Do you have any
justification for this?



----------------------- REVIEW 3 ---------------------
SUBMISSION: 15
TITLE: Towards the more perfect union type
AUTHORS: Michal Gajda

----------- Overall evaluation -----------
SCORE: -2 (reject)
----- TEXT:
Summary
=======

The paper proposes a modular framework for inferring type mappings from sample
data by viewing it as a learning problem from multiple examples. It evaluates
its performance on inferring Haskell data types from JSON API examples. Most
of its development is done though examples and code snippets.

Evaluation
==========

The paper attacks a problem (type inference) which has been studied in various
different contexts and languages in the past. Its title (".. more perfect
union type") is very general and quite grandiose, even though its focus is in
a very specific (and narrow) context, that of inferring types for tree-like
values (JSON data structures).

In my opinion, the paper requires significant rewrite (primarily) and
extension to be appreciated from the WFLP audience. Its language, esp. in the
type terminology, is confusing and it's not at all clear how (or whether) its
method improves against the state-of-the-art, not only in type inference in
general, but even in its very narrow focus of inferring types for JSON
structures. For example, no comparison with similar tools or any other kind of
evaluation is done.  In fact, the paper does not report any experimental
results (e.g., whether the approach is effective in the contexts it has been
applied, whether it scales, etc.) at all.

I do not really see what the WFLP audience (and its readers) will learn from
this...

Details
=======

13: Typing dynamic languages is a problem that has been studied and with
    solutions that have been proposed even earlier than the work of Anderson
    et al. [5] for Javascript.  In fact, it goes back much earlier than the
    time that Javascript was conceived.

28: "and is given shape inference algorithm": Not clear what this means.

58: "nor" -> "or"

59: It is unclear what C's undefined behaviour has to do with your paper...

- Delete Footnote 3.  In general, your paper has way too many unnecessary,
  and often distracting attention, footnotes.

160: The ending ":" does not make sense.

Various statements in the paper read a bit weird and/or are unclear. For
example, "We describe the laws as Quickcheck properties so that unit testing"
(page 5) or "type safety is quickly tested as equational laws with Quickcheck"
(page 15). (Quickcheck does random property-based testing, not unit testing,
and one cannot determine type safety using it -- or at least I do not
understand what you are trying to say there.)

