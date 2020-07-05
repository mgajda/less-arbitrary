---
title:  "Towards a more perfect union type"
shorttitle: "Perfect union type"
author:
  - name: Michał J. Gajda
    orcid:       "0000-0001-7820-3906"
affiliation:
  institution: "Migamake Pte Ltd"
  email:        mjgajda@migamake.com
  url:         "https://migamake.com"
#review: true
tags:
  - Haskell
  - JSON
  - union type
  - type providers
abstract: |
  We present a principled theoretical
  framework for dealing with union types,
  and show its work in practice
  on JSON data structures.

  The framework poses a union type
  inference as a learning problem
  from multiple examples.
  The categorical framework is
  generic, and easily extensible.
date:   2020-06-19
description: |
  Proposes a new framework for union types
  that uses monoid and complete functions,
  and enjoys easier theoretical treatment
  than ad-hoc typing relations.
link: "https://gitlab.com/migamake/json-autotype"
bibliography:
  - towards-better-union.bib
csl: acm-template/acm-sig-proceedings.csl
prologue: |
  ```{=latex}
  \usepackage[utf8]{inputenc}
  \usepackage{newunicodechar}
  \newunicodechar{∀}{\ensuremath{\forall{}}}
  %\DeclareUnicodeCharacter{2200}{\ensuremath{\forall{}}}
  ```
link-citations: true
tables: true
#listings: true
acks: |
  The author thanks for all tap-on-the-back donations to his past projects.

  The text was prepared with great help of bidirectional literate programming[@literate-programming] tool[@entangled],
  Pandoc[@pandoc] markdown publishing system and live feedback from GHCid[@ghcid].
---

Introduction
============

Typing dynamic languages has been long considered a challenge
[@javascript-inference]. The importance of the task grown with
the ubiquity cloud application programming
interfaces (APIs) utilizing JavaScript object notation (JSON),
where one needs to infer the structure having only a limited number of sample documents available.
Previous research have suggested it is possible to infer adequate type mappings
from sample data
[@json-autotype-prezi; @quicktype; @type-providers-f-sharp].

In the present study, we expand on these results. We propose a framework for type systems
in programming languages as learning algorithms, formulate it mathematically, and evaluate its
performance on JSON API examples.
The proposed framework is grounded on mathematical theory, and complete typing relation.
It is intended to add new features easily.

Related work
------------

### Union type providers

The earliest practical effort to apply union types to JSON inference to
generate Haskell types[@json-autotype-prezi].
It uses union type theory, but it also lacks an extensible theoretical framework.
F\# type providers for JSON facilitate deriving a schema
automatically; however, a type system is *ad-hoc*[@type-providers-f-sharp].
The other attempt to automatically infer schemas has been introduced in the PADS project
[@pads]. Nevertheless, it has not specified a generalized type-system design methodology.
One approach uses Markov chains to derive JSON types [@quicktype].
This approach requires considerable engineering time due to the implementation
unit tests in a case-by-case mode, instead of formulating laws applying to all types.
Moreover, this approach lacks a sound underlying theory.
Regular expression types were also used to type XML documents[@xduce], which does not allow
for selecting alternative representation.
Therefore, we summarize that there are several previously introduced approaches that provide partially
satisfactory results. In the present study, we aim to expand these proposals to enable systematic
addition of features, and automatic validation of types.


### Frameworks for describing type systems

Type systems are commonly expressed as partial relation of *typing*.
Their properties, such as subject reduction are also expressed relatively to the relation (also
partial) of *reduction* within a term rewriting system.
General formulations have been introduced for the Damas-Milner
type systems parameterized by constraints [@HM-X,@Jones].
It is also worth noting that traditional Damas-Milner type disciplines enjoy decidability,
and embrace the laws of soundness, and subject-reduction.
However these laws often prove too strict during type system extension,
dependent type systems often abandon subject-reduction,
and practical programming language type systems are either undecidable[@GHCZurihac],
or even sometimes even unsound[@typescript-soundness].

Early approach approach used lattice structure on the types [@subtyping-lattice],
which is more stringent that ours, since it requires idempotence
of unification (as join operation), as well as complementary meet operation
with the same properties.
Semantic subtyping approach provides a characterization
of set-based union, intersection, and complement types[@semantic-subtyping; @semantic-subtyping2],
which allows model subtype containment on first order types and functions.
This model relies on building a model using infinite sets in a set theory,
and thus is not immediately apparent as implementable^[Since we are interested in engineering the type system further, a simple implementation serves as ultimate evidence of extensibility.].
We are also not aware of a type inference framework that consistently
and completely preserve information in face of inconsistencies nor errors,
beyond using `bottom` and expanding to *infamous undefined
behaviour*[@undefined1,@undefined2,@undefined3].

We propose a categorical and constructive framework that preserves the soundness in inference,
while making compromises are allowed by consistent approximations.
Indeed our experience is that most of the type system implementation
may be generic. 

# Motivation {#sec:examples}

Here, we consider several examples paraphrased from JSON API descriptions.
We provide these examples in the form of a few JSON objects,
along with desired representation as Haskell data declaration.

1.  Subsets of data within a single constructor:

    a.   *API argument is an email* -- it is a subset of valid `String`
        values, that can be validated on the client side.
    b.   *The page size determines the number of results to return (min: 10,
            max:10,000)* -- it is also a subset of integer values (`Int`) between $10$,
            and $10,000$
    c. _The `date` field contains ISO8601 date_ -- a record field is represented
            as a `String` that contains a calendar date in the format
            `"2019-03-03"`


```{.json language="JSON" .hidden}
"amy@example.com"
"robert@example.com"
```

```{.haskell #representation-examples .hidden}
example1a_values :: [Value]
example1a_values = String <$> [
    "amy@example.com"
  , "edward@example.com"
  ]
example1a_repr :: HType
example1a_repr = HRef "Email"
```

``` {.haskell language="Haskell" file=test/example1a.result .hidden}
newtype Example1a = Example Email
```

``` {.json language="JSON" file=test/example1a.json .hidden}
{"message": "Where can I submit my proposal?",
    "uid" : 1014}
{"error"  : "Authorization failed",
   "code" : 401}
```

```{.haskell #representation-examples .hidden}
-- FIXME: this example belongs elsewhere!
not_example1a_values :: [String]
not_example1a_values  =
  ["{'message': 'Where can I submit my proposal?' ,'uid' : 1014}"
  ,"{\"error\"  : \"Authorization failed\", \"code\" : 401}"]

not_example1a_repr :: HType
not_example1a_repr = HRef "Email"
```

```{.json language="JSON" .hidden}
10
10000
```

``` {.json language="JSON" file=test/example1b.json .hidden }
{"example": [
  10,
  10000
]}
```

``` {.haskell file=test/example1b.result .hidden }
newtype Example = Example Int
```

```{.haskell #representation-examples .hidden }
example1b_values = Number <$> [
    10
  , 10000
  ]
example1b_repr = HRef "Int"
```
``` {.json file="test/example1c.json" .hidden}
"2019-03-03"
"2019-10-07"
```

``` {.haskell file=test/example1c.result .hidden }
newtype Example1c = Example1c Date
```

```{.haskell #representation-examples .hidden }
example1c_values = String <$> [
    "2019-03-03"
  , "2019-10-07"
  ]
example1c_repr = HRef "Date"
```
2.  Optional fields: *The page size is equal to 100 by default* --
    it means we have a record `{"page_size": 50}`
    or an empty record that should be interpreted as default value `{}`

``` {.json file=test/example2.json .hidden}
{}
{"page_size": 50}
```

```{.haskell file=test/example2.result .hidden}
newtype Example2 = Example2 { page_size :: Maybe Int }
```

```{.haskell #representation-examples .hidden }
example2_values :: [Value]
example2_values = fromMaybe (error "failed in example2")
                . decodeStrict <$> [
    "{}"
  , "{\"page_size\": 50}"
  ]
example2_repr = HADT [
    HCons "" [
      ("page_size", HApp "Maybe" ["Int"])
    ]
  ]
```

3.  Variant fields: *Answer to a query is either a number of registered objects, or
    String `"unavailable"`* - this is integer value (`Int`) or a `String` (`Int :|: String`)

``` {.haskell file=test/example3.result .hidden}
newtype Example3 = Example3 (String :|: Int)
```

```{.haskell #representation-examples .hidden }
example3_values = [
    String "alpha"
  , Number 10
  ]
example3_repr =
  HApp ":|:" [HRef "String", HRef "Int"]
```
4.  Variant records: *Answer contains either a text message with an user id, or an error.* --
    That is can be represented as one of following options:

``` {.json file=test/example4.json}
{"message" : "Where can I submit my proposal?", "uid" : 1014}
{"message" : "Submit it to HotCRP",             "uid" :  317}
{"error"   : "Authorization failed",            "code":  401}
{"error"   : "User not found",                  "code":  404}
```

```{.haskell #representation-examples .hidden }
example4_values :: [Value]
example4_values = fromJust . decodeStrict <$> [
    "{\"message\" : \"Where can I submit my proposal?\", \"uid\"  : 1014}"
  , "{\"message\" : \"Submit it to HotCRP\",             \"uid\"  :  317}"
  , "{\"error\"   : \"Authorization failed\",            \"code\" :  401}"
  , "{\"error\"   : \"User not found\",                  \"code\" :  404}"
  ]
example4_repr = HADT [
    HCons "" [("message", HRef "String")
             ,("uid",     HRef "Int"   )]
  , HCons "" [("error",   HRef "String")
             ,("code",    HRef "Int"   )] ]
```

```{.haskell file=test/example4.result}
data Example4 = Message { message :: String, uid  :: Int }
              | Error   { error   :: String, code :: Int }
```

5.  Arrays corresponding to records:

``` {.json file=test/example5.json #lst:row-constraint}
[ [1, "Nick",    null       ]
, [2, "George", "2019-04-11"]
, [3, "Olivia", "1984-05-03"] ]
```

``` {.haskell .hidden file=test/example5.result}
data Examples = Examples [Example]

data Example5 = Example5 { col1 :: Int
                         , col2 :: String
                         , col3 :: Maybe Date }
```

```{.haskell #representation-examples .hidden }
example5_values :: [Value]
example5_values = [readJSON $(embedFile "test/example5.json")]
example5_repr = HADT [
    HCons "" [("", HRef "Int"   )
             ,("", HRef "String")
             ,("", HRef "Date"  )] ]
```
6.  Maps of identical objects (example from [@quicktype]):

``` {.json file=test/example6.json}
{   "6408f5": { "size":       969709
              , "height":     510599
              , "difficulty": 866429.732
              , "previous":   "54fced"   },
    "54fced": { "size":       991394
              , "height":     510598
              , "difficulty": 866429.823
              , "previous":   "6c9589"   },
    "6c9589": { "size":       990527
              , "height":     510597
              , "difficulty": 866429.931
              , "previous":   "51a0cb"   } }
```

```{.haskell #representation-examples .hidden }
example6_values :: [Value]
example6_values = [readJSON $(embedFile "test/example6.json")]

example6_repr = HApp
     "Map"
    ["String"
    ,HADT [
     HCons "" [("size",       HRef "Int"   )
              ,("height",     HRef "Int"   )
              ,("difficulty", HRef "Double")
              ,("previous",   HRef "String")]
    ]]
```

It should be noted that the last example presented above requires
Haskell representation inference to be non-monotonic,
as a dictionary with a single key would have an incompatible type:

``` {.haskell file=test/example6-single-key.result}
data Example = Example { f_6408f5 :: O_6408f5, f_54fced :: O_6408f5
                       , f_6c9589 :: O_6408f5 }
data O_6408f5 = O_6408f5 { size, height :: Int, difficulty :: Double
                         , previous     :: String }
```

It also suggests that a user might decide to explicitly add evidence for
one of alternative representations in the case when samples are insufficient.
(like in case of a single element dictionary.)

::: { #sec:nonmonotonic-inference }

```{.haskell file=test/example6-multi-key.result}
data ExampleMap = ExampleMap (Map Hex ExampleElt)
data ExampleElt = ExampleElt { size       :: Int
                             , height     :: Int
                             , difficulty :: Double
                             , previous   :: String }
```

:::

Goal of inference
-----------------

Given an undocumented (or incorrectly labelled) JSON API, we may need to
read the input of Haskell encoding and avoid checking for the presence
of *unexpected* format deviations.
At the same time, we may decide to accept all known
valid inputs outright so that we can use types[^4] to ensure
that the input is processed exhaustively.

Accordingly, we can assume that the smallest non-singleton set is a better
approximation type than a singleton set. We call it *minimal containing set
principle*.

Second we can prefer types that allow for a fewer number of *degrees of freedom*
compared with the others, while conforming to a commonly occurring structure. We
denote it as an *information content principle*.

Given these principles, and examples of frequently occurring
patterns, we can infer a reasonable *world of types* that can be used as
approximations, instead of establishing this procedure in an ad-hoc
manner. In this way,we can implement *type
system engineering*, that allows deriving type system design
directly from the information about data structures
and the likelihood of their
occurrence.

Problem definition
==================

As we focus on JSON, we utilize Haskell encoding of the JSON term
for convenient reading(from Aeson package [@aeson]); specified as follows:

``` {.haskell file=refs/Data/Aeson.hs}
data Value = Object (Map String Value) | Array [Value] | Null
           | Number Scientific         | String Text   | Bool Bool
```
``` {.haskell file=refs/Data/Scientific.hs .hidden}
-- To incorporate both integers and exact decimal fractions[^6] in
-- the considered number representation, we employ decimal floating point[@scientific]:
data Scientific = Scientific { coefficient    :: Integer
                             , base10Exponent :: Int }
```

Defining type inference
-----------------------

### Information in the type descriptions

If an inference fails, it is always possible to correct it by introducing
an additional observation (example).
To denote unification operation, or **information fusion** between two type descriptions,
we use a `Semigroup` interface operation `<>` to merge types inferred
from different observations. If `Typelike` is semilattice, then `<>` is meet operation (least upper bound).
Note that this approach is dual to traditional unification that _narrows down_ solutions
and thus is join operation (greatest lower bound).
We use neutral element of the `Monoid` to indicate
a type corresponding to no observations.

``` {.haskell file=refs/Data/Semigroup.hs}
class Semigroup ty              where (<>)   :: ty -> ty -> ty
class Semigroup ty => Monoid ty where mempty :: ty
```

In other words, we can say that `mempty` (or `bottom`) of the corresponds to situation wher **no information was accepted** about a possible
value (no term seen, not even a null). It is neutral element of `Typelike`.
For example, an empty array `[]`
can be referred to as an array type with `mempty` as an element type.
This represents the view that `<>` always **gathers more information** about the type,
as opposed to the traditional unification that always **narrows down** possible solutions.
We describe the laws as QuickCheck [@quickcheck] properties so that
unit testing can be implemented to detect obvious violations.

### Beyond set

In the domain of permissive union types, a `beyond` set represents
the case of **everything permitted** or a fully dynamic value, when we gather
the information that permits every possible value inside a type. At the first
reading, it may be deemed that a `beyond` set should comprise of
only one single element -- the `top` one (arriving at complete bounded semilattice), but this is to narrow for our purpose
of _monotonically gathering information_


However, since we defined **generalization** operator `<>` as
**information fusion** (corresponding to unification in categorically dual case of a strict type systems.), we may encounter difficulties in assuring that no
information has been lost during the generalization^[Examples will be provided later.].
Moreover, strict type systems usually specify more than one error value,
as it should contain information about error messages and to keep track
from where an error has been originated[^7].

This observation lets us go well beyond typing statement of gradual type inference
as discovery problem from partial information[@gradual-typing].
Here we consider type inference as a **learning problem**,
and allows finding the common ground between the dynamic and the static typing
discipline. The languages relying on the static type discipline usually consider `beyond` as a set of
error messages, as a value should correspond to a statically assigned and
a **narrow** type. In this setting `mempty` would be fully polymorphic type $\forall{}a. a$.

Languages with dynamic type discipline will treat `beyond` as untyped,
dynamic value, and `mempty` again is a fully unknown, polymorphic value (like a type of an element of an empty array)[^8].

```{.haskell #typelike }
class (Monoid t, Eq t, Show t) => Typelike t where beyond :: t -> Bool
```

In addition, the standard laws for a **commutative** `Monoid`, we state
the new law for the `beyond` set: The `beyond` set is always **closed to information
addition** by `(<>a)` or `(a<>)` for any value of `a`. 
In other words the `beyond` set is an attractor of `<>` on both sides [^10].
However we do not require _idempotence_ of `<>`, which is uniformly present
in union type frameworks based on lattice[@subtyping-lattice]
and set-based approaches^[Which use Heyting algebras, which have more assumptions that the lattice approaches.][@semantic-subtyping].
Concerning union types, the key property of the `beyond` set, is that it is closed to
information acquisition:

``` {.haskell #typelike-spec .hidden}
beyond_is_closed :: forall   ty.
                    Typelike ty
                 => ty -> ty -> Property
```
``` {.haskell #typelike-spec .hidden}
beyond_is_closed ty1 ty2 = do
  if beyond (ty1 :: ty)
     then label "beyond"     $ (ty1 <> ty2) `shouldSatisfy` beyond
     else label "not beyond" $ True         `shouldBe`      True
```
```{.haskell #typelike-spec .hidden}
typelikeLaws :: (Arbitrary a
                ,Typelike  a)
             =>  Proxy     a
             ->  Laws
typelikeLaws (Proxy :: Proxy a) =
  Laws "Typelike"
    [("beyond is closed",
      property $ beyond_is_closed @a)]
-- We describe laws as QuickCheck properties~[@quickcheck] so that unit
-- testing can detect obvious violations.
```

In this way, we can specify other elements of `beyond` set instead of a single `top`.
When under strict type discipline, like that of Haskell[@GHCZurihac], we seek to enable each element of the `beyond` set
to contain at least one error message.[^9]

We abolish the semilattice requirement
that has been conventionally assumed for type constraints
[@subtype-inequalities], as this requirement is valid only for strict type
constraint inference, not for a more general type inference considered
as a learning problem. As we observe in the example 5
in [@sec:examples], we need to perform non-monotonic step of choosing alternative representation
after monotonic steps of merging all the information.

When a specific instance of `Typelike` is also a semilattice (an idempotent semigroup),
we will explicitly indicate if that is the case.
It is convenient validation when testing a recursive structure of the
type.
Note that we abolish semilattice requirement that was traditionally
assumed for type constraints here[@subtyping-lattice].
That is because this requirement is valid only for strict type
constraint inference, not for a more general type inference as a
learning problem. As we saw on `ExampleMap`
in [@sec:examples], we need non-monotonic
inference when dealing with alternative representations.
It should be noted that this approach significantly generalized the assumptions compared
with a full lattice subtyping [@subtype-inequalities;@subtyping-lattice].

Now we are ready to present **relation of typing** and its laws.
In order to preserve proper English word order, we state that $\mathit{ty}\,{}^\backprime{}\mathit{Types}^\backprime{}\,\mathit{val}$
instead of classical $\mathit{val}\mathrm{:}\mathit{ty}$.
Specifying the laws of typing is important, since we may need to consider separately
the validity of a domain of types/type constraints, and that of the sound typing of the
terms by these valid types.
The minimal definition of typing inference relation and type checking relation
is formulated as consistency between these two operations.


``` {.haskell #typelike }
class Typelike ty => ty `Types` val where
   infer ::       val -> ty
   check :: ty -> val -> Bool
```

``` {#types-spec .haskell .hidden}
mempty_contains_no_terms
  :: forall    ty term.
     (Typelike ty
     ,Types    ty term)
  =>              term
  -> Bool
```
``` {#types-spec .haskell .hidden}
mempty_contains_no_terms term =
      check (mempty :: ty) term
        == False
```

First, we note that to describe *no information*, `mempty` cannot
correctly type any term.
Second important rule of typing is that all terms are typed successfully by any
value in the `beyond` set. 
Finally we state the most intuitive rule for typing: a type inferred from a term, must
always be valid for that particular term.
The law asserts that the following diagram commutes:

``` {#types-spec .haskell .hidden}
beyond_contains_all_terms :: forall ty    term.
                            (Types  ty    term
                            ,Show         term)
                          =>        ty -> term
                          -> Property
beyond_contains_all_terms ty   term = do
  if beyond               ty
     then label "beyond"     $ term `shouldSatisfy` check ty
     else label "not beyond" $ True `shouldBe`      True
```

``` {#types-spec .haskell .hidden}
-- However, randomly drawing types for particular instances we might almost never get a type from the `beyond` set.
-- In this case, we can use special generator called `arbitraryBeyond` that generates only the elements of the `beyond`
-- set:
beyond_contains_all_terms2 :: forall          ty term.
                             (Typelike        ty
                             ,Types           ty term
                             ,ArbitraryBeyond ty     )
                           =>                    term
                           -> Property
beyond_contains_all_terms2 term =
  Test.QuickCheck.property $
    LessArbitrary.sizedCost $
      LessArbitrary.forAll (arbitraryBeyond @ty) $
        pure . (`check` term)
```

``` {#types-spec .haskell .hidden}
inferred_type_contains_its_term ::
     forall ty         term.
            ty `Types` term
  =>                   term
  -> Bool
inferred_type_contains_its_term term =
  check ((infer:: term -> ty) term) (term :: term)
```

```{=latex}
\begin{figure*}[t]
\begin{center}
% https://tikzcd.yichuanshen.de/#N4Igdg9gJgpgziAXAbVABwnAlgFyxMJZAJgBpiBdUkANwEMAbAVxiRAB12BbOnAC1zAAKgE80MAL4B9AIwAeAHyce-QaPHTiICaXSZc+QigAM5KrUYs2y3gJzCxk2dt0gM2PASIAWM9XrMrIgc3LZqjpoueh6GRGTe5gFWwTaq9kIATiwSUW76nkYkpMaJlkEhKnYOGrIABJx4XPD1oWnVTlo60QZeKL4J-mXWrVUAamWRXXkxvcimAxaBw5WC44HSMtrmMFAA5vBEoABmGRBcSDLUOBBIAMzUAEYwYFB3piBwAkc4SO8MWGBypBASBqHwYHRXsFgawrnQsAw2DDQSAGHQngwAAr5WLBAHYWC5E5nX5XG6Ie6LZIVML2ADG4LpAGtOLUkh0UU8XkhvABOKbE86Id7XO6PZ5QgC0fMGSxSI0EDJgzNZ7I2KLRGOxMyMIAyWF2fB+AtOQrIIFFFJNJMQlwt5Nu1qFdst73ZbEUqSq6g5TqQ5tdsupXvCNXkCg16JgWJxvT1BqNRNNPLJpKp5UUSZtvnt-qD5U4aCwUi01E10e1PV1DBg3yzQpzlrt7vlRecZajMZ1bH1huNrkFSAAbKnbfnlrTgACjjAMjk-YgAKyj80tmltaeznIdrWx3W9xML5e5inj+UremMlnsWoAd1wfAkrJ9Jfrw9HlLXIcvyuvd4fT43i+mw7hWe49gmxoUBIQA
\begin{tikzcd}
                                                                                              &  & \mathit{Type}_1 \times \mathit{Type}_2 \arrow[dd, "<>"] \arrow[rrdd, "\pi_2"] \arrow[lldd, "\pi_1"']                                       &  &                                                                                           \\
                                                                                              &  &                                                                                                                                            &  &                                                                                           \\
\mathit{Type}_1 \arrow[rrdd, phantom, bend left] \arrow[rrdd] \arrow[rr, "<>\mathit{Type}_2"] &  & \mathit{Type}_1<>\mathit{Type}_2 \arrow[dd, "\mathit{check\ value}_2", bend left=49] \arrow[dd, "\mathit{check\ value}_1"', bend right=49] &  & \mathit{Type}_2 \arrow[lldd] \arrow[ll, "\mathit{Type}_1<>"']                             \\
                                                                                              &  &                                                                                                                                            &  &                                                                                           \\
\mathit{Value}_1 \arrow[uu, "\mathit{infer}"] \arrow[rr, "\mathit{check\ with}\ Type_1"']     &  & \mathit{True}                                                                                                                              &  & \mathit{Value}_2 \arrow[uu, "\mathit{infer}"'] \arrow[ll, "\mathit{check\ with}\ Type_2"]
\end{tikzcd}
\end{center}
\caption{Categorical diagram for \texttt{Typelike}.}
\end{figure*}
```

``` {#fig:type-commutes .dot width="45%" height="20%" .hidden}
digraph type {
  node [shape=box,color=white];

  subgraph types {
    Type1  [label=<Type<SUB>1</SUB>>];
    Type2  [label=<Type<SUB>2</SUB>>];
    rank=same;
  }
  TypeM  [label=<Type<SUB>1+2</SUB>>];
  Bool;

  subgraph sinks {
    Value1 [label=<Value<SUB>1</SUB>>];
    Value2 [label=<Value<SUB>2</SUB>>];
    rank=sink;
  }
  Type1 -> TypeM [label=<+Type<SUB>2</SUB>>]
  Type2 -> TypeM [label=<+Type<SUB>2</SUB>>]
  Value1 -> TypeM [style=invis,weight=2];
  Value2 -> TypeM [style=invis,weight=2];

  Value1 -> Type1 [label="infer"];
  Value2 -> Type2 [label="infer"];
  Type1  -> Bool [label=<check<BR/>Value<SUB>1</SUB>>];
  Type2  -> Bool [label=<check<BR/>Value<SUB>2</SUB>>];
  Value1 -> Bool [label="const True"];
  Value2 -> Bool [label="const True"];
  TypeM  -> Bool [label=<check<BR/>Value<SUB>1</SUB>>];
  TypeM  -> Bool [label=<check<BR/>Value<SUB>2</SUB>>];
}
```


```{.haskell #types-spec .hidden}
fusion_keeps_terms :: forall   ty v.
                     (Typelike ty
                     ,ty `Types` v)
                   => v -> ty -> ty -> Property
```
```{.haskell #types-spec .hidden}
fusion_keeps_terms v ty1 ty2 = do
  if | check ty1 v -> label "first type checks value"  $ (ty1 <> ty2) `shouldSatisfy` (`check` v)
     | check ty2 v -> label "second type checks value" $ (ty1 <> ty2) `shouldSatisfy` (`check` v)
     | otherwise   -> label "no type checks value"     $ True `shouldBe` True
```

The last law states that the terms are correctly typechecked
after adding more information into a single type.
(For inference relation, it would be described as _principal type property_.)
The minimal `Typelike` instance is the one that contains only `mempty`
corresponding to the case of *no sample data received*, and a single `beyond` element for *all values permitted*.
We will define it below as `PresenceConstraint` in [@sec:presence-absence-constraints].
It should be noted that these laws are still compatible with the strict, static type
discipline: namely the `beyond` set corresponds to a set of constraints with at least one
type error, and a task
of a compiler to prevent any program with the terms that type only to
the `beyond` as a least upper bound.

Type engineering principles
---------------------------

Considering that we aim to infer a type from a finite number of samples,
we are presented with a *learning problem*, so we need to use *prior*
knowledge about the domain for inferring types.
Observing that  $a: \text{false}$ we can expect that in particular cases,
we may obtain that $a : \text{true}$.
After noting that $b: 123$, we expect that $b: 100$ would also be
acceptable. It means that we need to consider a typing system to *learn a reasonable
general class from few instances*. This motivates formulating the type system 
as an inference problem.
As the purpose is to deliver the most descriptive[^11] types, we assume
that we need to obtain a wider view rather than focusing on a *free type*
and applying it to larger sets whenever it is deemed justified.

The other principle corresponds to **correct operation**. It implies
that having operations regarded on types,
we can find a minimal set of types that assure correct
operation in the case of unexpected errors.
Indeed we want to apply this theory to infer a type definition from a
finite set of examples. We also seek to generalize it to infinite
types.
For this purpose, we set the rules of type design as short description as possible,
inference must be a **contravariant** functor with regards to constructors.
For example, if `AType x y` types `{"a": X, "b": Y}`, then
`x` must type `X`, and `y` must type `Y`.

## Constraint definition

### Flat type constraints

Let us first consider typing of flat type: `String` (similar treatment should be given to the `Number`.type.)

```{.haskell #basic-constraints .hidden}

data IntConstraint = IntRange Int Int
                   | IntNever
                   | IntAny
  deriving (Show, Eq, Generic)

instance Semigroup IntConstraint where
  IntAny       <> _            = IntAny
  _            <> IntAny       = IntAny
  IntNever     <> a            = a
  a            <> IntNever     = a
  IntRange          a         b  <>
    IntRange          c         d =
      IntRange (min a c) (max b d)

instance Typelike IntConstraint where
  beyond = (==IntAny)

instance Monoid IntConstraint where
  mempty = IntNever

instance IntConstraint `Types` Int where
  infer                i = IntRange i i
  check  IntNever      _ = False
  check  IntAny        _ = True
  check (IntRange a b) i = a <= i && i <= b

--JavaScript provides one number type that contains both `Float` and `Int`,
--so that the JSON values inherit this type:

data NumberConstraint =
    NCInt
  | NCNever
  | NCFloat
  deriving(Eq,Show,Generic)

instance Semigroup NumberConstraint where
  NCFloat <> _       = NCFloat
  _       <> NCFloat = NCFloat
  NCNever <> a       = a
  a       <> NCNever = a
  NCInt   <> NCInt   = NCInt

instance Typelike NumberConstraint where
  beyond = (==NCFloat)

instance NumberConstraint `Types` Scientific where
  infer sci
    | base10Exponent sci >= 0 = NCInt
  infer _                     = NCFloat
  check NCFloat _   = True
  check NCInt   sci = base10Exponent sci >= 0
  check NCNever _   = False

instance Monoid NumberConstraint where
  mempty = NCNever
```

``` {#basic-constraints .haskell}
data StringConstraint = SCDate               | SCEmail
  | SCEnum (Set Text) | SCNever {- mempty -} | SCAny {- beyond -}
```
``` {#basic-constraints .haskell .hidden}
  deriving(Eq, Show,Generic)
```

``` {#basic-constraints .haskell}
instance StringConstraint `Types` Text where
  infer (isValidDate  -> True) = SCDate
  infer (isValidEmail -> True) = SCEmail
  infer  value                 = SCEnum $ Set.singleton value
  infer  _                     = SCAny

  check  SCDate     s = isValidDate  s
  check  SCEmail    s = isValidEmail s
  check (SCEnum vs) s = s `Set.member` vs
  check  SCNever    _ = False
  check  SCAny      _ = True

instance Semigroup StringConstraint where
  SCNever    <>  a                                = a
  a          <>  SCNever                          = a
  SCAny      <>  _                                = SCAny
  _          <>  SCAny                            = SCAny
  SCDate     <>  SCDate                           = SCDate
  SCEmail    <>  SCEmail                          = SCEmail
  (SCEnum a) <> (SCEnum b) | length (a `Set.union` b) <= 10 = SCEnum (a <> b)
  _          <>  _                                = SCAny
```

``` {#basic-constraints .haskell .hidden}
instance Monoid StringConstraint where
  mempty  = SCNever

instance Typelike StringConstraint where
  beyond  = (==SCAny)
```


### Free union type

Before we endavour on finding type constraints for compound values (arrays and objects),
it might be instructive to find a notion of _free type_, that is a type with no
additional laws but the ones stated above.
Given a term with arbitrary constructors we can infer a _free type_ for every
term set $T$ as follows: For any $T$ value type $\mathit{Set}\ T$ satisfies our notion of *free
type* specified as follows:

``` {#freetype .haskell}
data FreeType a = FreeType { captured :: Set a } | Full
```
```{#freetype .haskell .hidden}
  deriving (Eq, Ord, Show, Generic)
```
``` {#freetype .haskell}
instance (Ord a, Eq a) => Semigroup (FreeType a) where
  Full <> _    = Full
  _    <> Full = Full
  a    <> b    = FreeType $ (Set.union `on` captured) a b
instance (Ord a, Eq a, Show a) => Typelike (FreeType a) where
  beyond = (==Full)

instance (Ord a, Eq a, Show a) => FreeType a `Types` a where
  infer                    = FreeType . Set.singleton
  check Full         _term = True
  check (FreeType s)  term = term `Set.member` s
```

```{#freetype .haskell .hidden}
instance (Ord a, Eq a)         => Monoid (FreeType a) where
  mempty = FreeType Set.empty
```

This definition is deemed sound, and may be applicable
to finite sets of terms or values.
For a set of values: `["yes", "no", "error"]`, we may
reasonably consider that type is an appropriate approximation of C-style
enumeration, or Haskell-style ADT without constructor arguments.
However, the deficiency of this notion of *free type* is that it does not allow
generalizing in infinite and recursive domains! It only allows utilizing objects from
the sample.

### Presence and absence constraint {#sec:presence-absence-constraints}

We call the degenerate case of `Typelike` a *presence or absence constraint*.
It just checks that the type contains at least one observation of the input value,
or no observations at all. It is important as it can be used
to specify an element type of an empty array.
After seeing `true` value we also expect `false`, so we can say that
it is also basic constraint for boolean values.
The same is valid for `null` values, as there is only one `null` value
to ever observe.

``` {#presence-absence-constraints .haskell}
type BoolConstraint       = PresenceConstraint Bool
type NullConstraint       = PresenceConstraint ()
data PresenceConstraint a = Present | Absent
```
``` {#presence-absence-constraints .haskell .hidden}
  deriving (Eq, Show, Typeable, Generic)

type role PresenceConstraint nominal

instance Semigroup (PresenceConstraint a) where
  Absent  <> a       = a
  a       <> Absent  = a
  Present <> Present = Present

instance Monoid (PresenceConstraint a) where
  mempty             = Absent

instance Typelike (PresenceConstraint a) where
  beyond    = (==Present)

instance PresenceConstraint a `Types` a where
  infer _         = Present
  check Present _ = True
  check Absent  _ = False
```

#### Variants

Variants of two mutually exclusive types are also simple. They can be implement them with a
type related to `Either` type that assumes these types are exclusive, we denote it by `:|:`.
In other words for `Int :|: String` type, we first control whether the value is
an `Int`, and if this check fails, we attempt to parse it as `String`.
Variant records are slightly more complicated, as it may be unclear which
typing is better to use:

``` {.haskell .hidden}
data a :|: b = AltLeft  a
             | AltRight b
  deriving (Show, Eq, Generic)

instance (FromJSON  a
         ,FromJSON        b)
      =>  FromJSON (a :|: b) where
  parseJSON a =  AltLeft  <$> decodeEither
             <|> AltRight <$> decodeEither
```
``` {.json .javascript file=test/example_variant1.json}
{"message": "Where can I submit my proposal?", "uid" : 1014}
{"error"  : "Authorization failed",            "code":  401}
```

``` {.haskell file=test/example_variant1.result}
data OurRecord = OurRecord { message, error :: Maybe String
                           , code,    uid   :: Maybe Int }
```
``` {.haskell file=test/example_variant2.result}
data OurRecord2 = Message  { message :: String, uid  :: Int }
                | Error    { error   :: String, code :: Int }
```

The best attempt here is to rely on the available examples being
reasonably exhaustive. That is, we can estimate how many examples we
have for each, and how many of them match. Then, we compare this number
with type complexity (with options being more complex
to process, because they need additional `case` expression.)
In such cases,
the latter definition has only one `Maybe` field (on the toplevel,
optionality is one),
while the former definition has four `Maybe` fields (optionality is four).
When we obtain more samples, the pattern emerges:

``` {.json file=test/example_variant2.json}
{"error"  : "Authorization failed",            "code":  401}
{"message": "Where can I submit my proposal?", "uid" : 1014}
{"message": "Sent it to HotCRP",               "uid" :   93}
{"message": "Thanks!",                         "uid" : 1014}
{"error"  : "Missing user",                    "code":  404}
```

#### Type cost function

Since we are interested in types with less complexity and less optionality,
we will define cost function as follows:

```{.haskell #typecost}
class Typelike ty => TypeCost ty where
  typeCost ::  ty -> TyCost
  typeCost a = if a == mempty then 0 else 1
instance Semigroup TyCost where (<>)   = (+)
instance Monoid    TyCost where mempty = 0

newtype TyCost = TyCost Int
```
```{.haskell #typecost .hidden}
  deriving (Eq, Ord, Show, Enum, Num)
```

When presented with several alternate representations
from the same set of observations, we will use this function
to select the least complex representation of the type.
For flat constraints as above, we infer that they
offer no optionality when no observations occured (cost of `mempty` is $0$),
otherwise the cost is $1$.
Type cost should be non-negative, and non-decreasing when we add new
observations to the type.

```{.haskell #typecost .hidden}
-- Considering that types `beyond` are to be avoided,
-- we can assign conceptual _infinity_ to these values.
-- For the implementation purposes we will represent
-- it by the value so high, that is unlikely to ever occur
-- in practical types, but still small enough that we
-- can add it without checking for overflow.

inf :: TyCost
inf = 100000000
```

```{.haskell #typecost-laws .hidden}
prop_typeCost_is_non_negative :: TypeCost ty
                              => ty -> Bool
prop_typeCost_is_non_negative ty =
  typeCost ty >= 0

prop_typeCost_is_non_decreasing :: TypeCost ty
                                => ty -> ty -> Bool
prop_typeCost_is_non_decreasing ty1 ty2 =
     typeCost ty1 <= typeCost (ty1<>ty2)
  && typeCost ty2 <= typeCost (ty1<>ty2)

typeCostLaws :: (TypeCost  ty
                ,Arbitrary ty)
             =>  Proxy     ty
             ->  Laws
typeCostLaws (Proxy :: Proxy ty)=
  Laws "TypeCost"
    [("non-negative"
     ,property $ prop_typeCost_is_non_negative   @ty)
    ,("non-decreasing"
     ,property $ prop_typeCost_is_non_decreasing @ty)]

```

```{.haskell #typecost .hidden}
instance TypeCost IntConstraint where
instance TypeCost NumberConstraint where
instance TypeCost StringConstraint where
instance TypeCost BoolConstraint where
instance TypeCost NullConstraint where
```

### Object constraint

To avoid information loss, a constraint for JSON object type is
introduced in such a way to **simultaneously gather information** about
representing it either as a `Map`, or a record.
The typing of `Map` would be specified as follows, with the optionality cost
being a sum of optionalities in its fields.

``` {.haskell }
data MappingConstraint = MappingNever -- mempty
   | MappingConstraint { keyConstraint   :: StringConstraint
                       , valueConstraint :: UnionType        }
instance TypeCost MappingConstraint where
  typeCost MappingNever           = 0
  typeCost MappingConstraint {..} = typeCost keyConstraint
                                  + typeCost valueConstraint
```
``` {#object-constraint .haskell .hidden}
data MappingConstraint = MappingNever -- mempty
   | MappingConstraint { keyConstraint   :: StringConstraint
                       , valueConstraint :: UnionType        }

  deriving (Eq, Show, Generic, Typeable)

instance TypeCost MappingConstraint where
  typeCost MappingNever           = 0
  typeCost MappingConstraint {..} = typeCost keyConstraint
                                  + typeCost valueConstraint

instance Monoid MappingConstraint where
  mempty = MappingNever

instance Typelike MappingConstraint where
  beyond MappingNever = False
  beyond MappingConstraint {..} =
       beyond keyConstraint
    && beyond valueConstraint

instance Semigroup   MappingConstraint where
  MappingNever <> a = a  
  a <> MappingNever = a  
  a <> b = MappingConstraint {
      keyConstraint   =
        ((<>) `on` keyConstraint  ) a b
    , valueConstraint =
        ((<>) `on` valueConstraint) a b
    }

instance MappingConstraint `Types`
         Object where
  infer obj = MappingConstraint
                (foldMap infer $ Map.keys obj)
                (foldMap infer            obj)
  check MappingNever           _   = False
  check MappingConstraint {..} obj =
       all (check keyConstraint)
           (Map.keys obj)
    && all (check valueConstraint)
           (Foldable.toList obj)
```

Separately, we acquire the information about
a possible typing of a JSON
object as a record of values.
Note that `RCTop` never actually occurs during inference.
That is, we could have represented the `RecordConstraint`
as a `Typelike` with an empty `beyond` set.
The merging of constraints would be simply merging of all column constraints.


``` {.haskell #object-constraint}
data RecordConstraint =
    RCTop {- beyond -} | RCBottom {- mempty -}
  | RecordConstraint { fields :: HashMap Text UnionType }
```
``` {.haskell #object-constraint .hidden}
    deriving (Show,Eq,Generic, Typeable)
```
``` {#object-constraint .haskell .hidden}
instance Semigroup   RecordConstraint where
  RecordConstraint     a  <>
    RecordConstraint     b = RecordConstraint $
    Map.unionWith (<>) a b
```

``` {#object-constraint .haskell .hidden}
  RCBottom <> a        = a
  a        <> RCBottom = a
  RCTop    <> _        = RCTop
  _        <> RCTop    = RCTop

instance Typelike RecordConstraint where
  beyond = (==RCTop)

instance Monoid      RecordConstraint where
  mempty = RCBottom

instance TypeCost RecordConstraint where
  typeCost RCBottom = 0
  typeCost RCTop    = inf
  typeCost RecordConstraint { fields } =
    Foldable.foldMap typeCost fields
```

``` {#object-constraint .haskell}
instance RecordConstraint `Types` Object where
    infer = RecordConstraint    . Map.fromList
          . fmap (second infer) . Map.toList
    check RecordConstraint {fields} obj =
         all (`elem` Map.keys fields) -- all object keys
                    (Map.keys  obj)        -- present in type
      && and (Map.elems $ Map.intersectionWith -- values check
                            check fields obj)
      && all isNullable (Map.elems $ fields `Map.difference` obj)
         -- absent values are nullable
```
```{.haskell .hidden #object-constraint}
    check RCTop    _ = True
    check RCBottom _ = False
    check _  _ = False
    -- FIXME: treat extra keys!!!
isNullable UnionType { unionNull = Present } = True
isNullable _                                 = False
```

Observing that the two abstract domains considered above are
independent, we can store the information about both options separately
in a record[^13]. It should be noted that this representation is similar to *intersection
type*: any value that satisfies `ObjectConstraint` must
conform to both `mappingCase`, and `recordCase`.
Also this *intersection approach* in order to address
alternative union type representations benefits from  *principal type property*,
meaning that a principal type is used to simply acquire the information
corresponding to different representations and handle it separately.
Since we plan to choose only one representation for the object,
we can say that minimum cost of this type is a minimum of component costs.

``` {#object-constraint .haskell}
data ObjectConstraint = ObjectNever -- mempty
  |  ObjectConstraint { mappingCase :: MappingConstraint
                      , recordCase  :: RecordConstraint } 
```
``` {#object-constraint .haskell .hidden}
  deriving (Eq,Show,Generic)

instance Semigroup ObjectConstraint where
  ObjectNever <> a = a
  a <> ObjectNever = a
  a <> b           =
    ObjectConstraint                            {
      mappingCase = ((<>) `on` mappingCase) a b
    , recordCase  = ((<>) `on` recordCase ) a b }

instance Monoid ObjectConstraint where
  mempty = ObjectNever

instance Typelike ObjectConstraint where
  beyond ObjectNever           = False
  beyond ObjectConstraint {..} =
       beyond mappingCase
    && beyond recordCase

instance ObjectConstraint `Types` Object where
  infer v = ObjectConstraint (infer v) (infer v)
  check ObjectNever           _ = False
  check ObjectConstraint {..} v = check mappingCase v
                               && check recordCase  v
```
```{.haskell #object-constraint}
instance TypeCost ObjectConstraint where
  typeCost ObjectConstraint {..} = typeCost mappingCase
                             `min` typeCost recordCase
```

```{.haskell #object-constraint .hidden}
  typeCost ObjectNever           = 0
```

### Array constraint

Similarly to the object type, `ArrayConstraint` is used to simultaneously
obtain information about all possible representations of an array,
differentiating between an array of the same elements,
and a row with the type depending on a column.
We need to acquire the information for both alternatives separately, and
then, to measure a relative likelihood of either cases, before mapping
the union type to Haskell declaration.
Here, we specify the records for two different possible representations:

``` {.haskell #array-constraint}
data ArrayConstraint = ArrayNever -- mempty
   | ArrayConstraint { arrayCase :: UnionType, rowCase :: RowConstraint }
```
``` {.haskell #array-constraint .hidden}
  deriving (Show, Eq, Generic)

instance Monoid ArrayConstraint where
  mempty = ArrayNever

instance Typelike ArrayConstraint where
  beyond ArrayNever = False
  beyond ArrayConstraint {..} =
       beyond arrayCase
    && beyond rowCase

instance Semigroup ArrayConstraint where
  ArrayNever <> a          = a
  a          <> ArrayNever = a
  a1 <> a2 =
    ArrayConstraint {
      arrayCase = ((<>) `on` arrayCase) a1 a2
    , rowCase   = ((<>) `on` rowCase  ) a1 a2
    }
```

Semigroup operation just merges information on the components,
and the same is done when inferring types or checking them:
For the arrays, we plan to again choose only one
of possible representations, so the cost of optionality
is the lesser of the costs of the representation-specific constraints.

``` {.haskell #array-constraint}
instance ArrayConstraint `Types` Array where
    infer vs = ArrayConstraint {
        arrayCase = mconcat (infer <$> Foldable.toList vs)
      , rowCase   =          infer                     vs }

    check ArrayNever           vs = False
    check ArrayConstraint {..} vs =
         and (check arrayCase <$> Foldable.toList vs)
      &&      check rowCase                       vs
```

```{.haskell #object-constraint .hidden}
instance TypeCost ArrayConstraint where
  typeCost ArrayNever = 0
  typeCost ArrayConstraint {..} =
    typeCost arrayCase `min`
    typeCost rowCase
```

### Row constraint

A row constraint is valid only if there is the same number of entries in
all rows, which is represented by escaping the `beyond` set
whenever there is an uneven number of columns.
Row constraint remains valid only if both
constraint describe record of the same length,
otherwise we yield `RowTop` to indicate that it is
no longer valid.
In other words, `RowConstraint` is a *levitated
semilattice*[@levitated-lattice]^[_Levitated lattice_ is created by appending distinct `bottom` and `top` to a set that does not possess them by itself.] with a neutral element over the content type
that is a list of `UnionType` objects.

``` {#row-constraint .haskell}
data RowConstraint = RowTop | RowNever | Row [UnionType]
```
``` {#row-constraint .haskell .hidden}
   deriving (Eq,Show,Generic)

instance Typelike RowConstraint where
  beyond = (==RowTop)

instance Monoid RowConstraint where
  mempty = RowNever
```

``` {#row-constraint .haskell .hidden}
instance RowConstraint `Types` Array where
  infer = Row
        . Foldable.toList
        . fmap infer
  check RowTop   _ = True
  check RowNever _ = False
  check (Row rs) vs
    | length rs == length vs =
      and $
        zipWith check                 rs
                     (Foldable.toList vs)
  check  _        _ = False
```

``` {#row-constraint .haskell .hidden}
instance Semigroup RowConstraint where
  Row bs    <> Row cs
    | length bs /= length cs = RowTop
  Row bs    <> Row cs        =
    Row $ zipWith (<>) bs cs
```


``` {#row-constraint .haskell .hidden}
  RowTop    <> _             = RowTop
  _         <> RowTop        = RowTop
  RowNever  <> a             = a
  a         <> RowNever      = a
```

```{.haskell #row-constraint .hidden}
-- The cost of the row constraint is inferred by summing up costs
-- as the cost of the record constraint.
instance TypeCost RowConstraint where
  typeCost  RowNever  = 0
  typeCost  RowTop    = inf
  typeCost (Row cols) = foldMap typeCost cols
```

### Combining the union type

It should note that given the constraints for the different type constructors,
the union type can be considered as mostly a generic `Monoid` instance[@generic-monoid].
Merging information with `<>` and `mempty` follow the pattern above, by just lifting
operations on the component.

``` {#type .haskell}
data UnionType = UnionType {
    unionNull :: NullConstraint,   unionBool :: BoolConstraint
  , unionNum  :: NumberConstraint, unionStr  :: StringConstraint
  , unionArr  :: ArrayConstraint,  unionObj  :: ObjectConstraint }
```
```{#type .haskell .hidden}
     deriving (Eq,Generic)

instance Semigroup UnionType where
  u1 <> u2 =
    UnionType {
      unionNull = ((<>) `on` unionNull) u1 u2
    , unionBool = ((<>) `on` unionBool) u1 u2
    , unionNum  = ((<>) `on` unionNum ) u1 u2
    , unionStr  = ((<>) `on` unionStr ) u1 u2
    , unionObj  = ((<>) `on` unionObj ) u1 u2
    , unionArr  = ((<>) `on` unionArr ) u1 u2
    }
```

```{.haskell #type .hidden}
instance Show UnionType where
  showsPrec _ UnionType {..} =
      showString "UnionType {"
    . showsElts [
        showsNonEmpty "unionNull" unionNull
      , showsNonEmpty "unionBool" unionBool
      , showsNonEmpty "unionNum"  unionNum
      , showsNonEmpty "unionStr"  unionStr
      , showsNonEmpty "unionObj"  unionObj
      , showsNonEmpty "unionArr"  unionArr
      ]
    . showString "}"

showsNonEmpty :: (Eq     a
                 ,Show   a
                 ,Monoid a)
              =>  String
              ->         a
              ->  Maybe ShowS
showsNonEmpty _    field | field == mempty = Nothing
showsNonEmpty name field                   = Just $
  showString name . showString "=" . showsPrec 0 field

showsElts :: [Maybe ShowS] -> ShowS
showsElts elts =
  case catMaybes elts of
    []     -> id
    (x:xs) -> compose $
     x:map (showString ", ".) xs
  where
    compose = foldr (.) id
```

The generic structure of union type can be explained by the fact that
the information contained in each record field is *independent*
from the information contained in other fields.
It means that we perform anti-unification independently over different
dimensions.

``` {#type .haskell .hidden}
instance Monoid UnionType where
  mempty = UnionType {
      unionNull = mempty
    , unionBool = mempty
    , unionNum  = mempty
    , unionStr  = mempty
    , unionObj  = mempty
    , unionArr  = mempty
    }
```


``` {#type .haskell .hidden}
-- As we described previously, the `beyond` set may correspond to either
-- **accepting any value** or to **accepting no more information**. Its
-- definition should be no surprise:
instance Typelike UnionType where
  beyond UnionType {..} =
      beyond unionNull
   && beyond unionBool
   && beyond unionNum
   && beyond unionStr
   && beyond unionObj
   && beyond unionArr
```

Inference breaks down disjoint alternatives corresponding to
different record fields, depending on the constructor of a given value.
It enables implementing a clear and efficient treatment
of different alternatives separately[^14]. Since union type is all about optionality, we need to sum all options
from different alternatives to obtain its `typeCost`.

``` {#union-type-instance .haskell}
instance UnionType `Types` Value where
  infer (Bool   b) = mempty { unionBool = infer b  }
  infer  Null      = mempty { unionNull = infer () }
  infer (Number n) = mempty { unionNum  = infer n  }
  infer (String s) = mempty { unionStr  = infer s  }
  infer (Object o) = mempty { unionObj  = infer o  }
  infer (Array  a) = mempty { unionArr  = infer a  }

  check UnionType { unionBool } (Bool   b) = check unionBool b
  check UnionType { unionNull }  Null      = check unionNull ()
  check UnionType { unionNum  } (Number n) = check unionNum  n
  check UnionType { unionStr  } (String s) = check unionStr  s
  check UnionType { unionObj  } (Object o) = check unionObj  o
  check UnionType { unionArr  } (Array  a) = check unionArr  a
```
```{.haskell #union-type-instance .hidden}
-- Since union type is all about optionality,
-- we need to sum all options from different alternatives:
instance TypeCost UnionType where
  typeCost UnionType {..} = typeCost unionBool
     + typeCost unionNull + typeCost unionNum
     + typeCost unionStr  + typeCost unionObj
     + typeCost unionArr
```

### Overlapping alternatives

The essence of union type systems have long been dealing with
the conflicting types provided in the input.
Motivated by the examples above, we also aim to address
conflicting alternative assignments.
It is apparent that examples 4. to 6. hint at more than one
assignment: in example 5, a set of lists of values that may correspond to `Int`, `String`, or
    `null`, or a table that has the same (and predefined) type for each
    values; in example 6 A record of fixed names or the mapping from hash to a single object
    type.

### Counting observations

In this section, we discuss how to gather information about the
number of samples supporting each alternative type constraint.
To explain this, the other example can be considered:

``` {.json}
{"history": [
   {"error"  : "Authorization failed",            "code":  401}
  ,{"message": "Where can I submit my proposal?", "uid" : 1014}
  ,{"message": "Sent it to HotCRP",               "uid" :   93}
  ,{"message": "Thanks!",                         "uid" : 1014}
  ,{"error"  : "Authorization failed",            "code":  401}]}
```

First, we need to identify it as a list of similar elements. 
Second, we note, that there are multiple instances of each record example. We
consider that the best approach would be to use the multisets of
inferred records instead of normal sets.
To find the best representation, we can a type complexity,
and attempt to minimize the term.
Next step is to detect the similarities between type descriptions
introduced for different parts of the term:

```{.json}
{"history"      : [...]
,"last_message" : {"message": "Thanks!", "uid" : 1014} }
```

We can add the auxiliary information about a number of samples observed,
and the constraint will remain a `Typelike` object.
The `Counted` constraint counts the number of samples observed for the constraint inside so that we can decide
on which alternative representation is best supported by evidence.
It should be noted that `Counted` constraint is the first example that
does not correspond to a semilattice, that is `a<>a`$\neq$`a`.
This is natural for a `Typelike` object; it is not
a type constraint in a conventional sense, just an accumulation of knowledge.

```{#counted .haskell}
data Counted a = Counted { count :: Int, constraint :: a }
```
```{#counted .haskell .hidden}
    deriving (Eq, Show, Generic)
```
```{#counted .haskell}
instance Semigroup a => Semigroup (Counted a) where
  a <> b = Counted (count      a +  count      b)
                   (constraint a <> constraint b)
```
```{#counted .haskell .hidden}
instance Monoid a => Monoid (Counted a) where
  mempty = Counted  0 mempty
```
``` {.haskell #counted .hidden}
instance Typelike          a
      => Typelike (Counted a) where
  beyond Counted {..} = beyond constraint

instance TypeCost          ty
      => TypeCost (Counted ty) where
  typeCost (Counted _ ty) = typeCost ty
```

``` {.haskell #counted .hidden}
instance          ty  `Types` term
      => (Counted ty) `Types` term where
  infer term = Counted 1 $ infer term
  check (Counted _ ty) term = check ty term
```


Therefore, at each step, we may need to maintain a **cardinality** of
each possible value, and being provided with sufficient number of
samples, we may attempt to detect[^15].
To preserve efficiency, we may need to merge whenever the number of
alternatives in a multiset crosses the threshold. We can attempt to
narrow strings only in the cases when cardinality crosses the threshold.

# Finishing touches

The final touch would be to perform the post-processing of an assigned
type before generating it to make it more resilient to common
uncertainties.
It should be noted that these assumptions may bypass the defined
least-upper-bound criterion specified in the initial part of the paper; however,
they prove to work well in practice.

If we have no observations corresponding to an array type, it can be
inconvenient to disallow an array to contain any values at all.
Therefore, we introduce a non-monotonic step of converting
the `mempty` into a final `Typelike` object aiming to
introduce a representation allowing the occurrence of any `Value` in the input.
That still preserves the validity of the typing.
We note that the program using our types must not have any assumptions about
these values; however, at the same time it should be able to print them for
debugging purposes.

``` {#fig:dataflow .dot width="48%" height="13%" .hidden}
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

In most JSON documents, we observe that the same object can be described
in different parts of sample data structures. Due to this reason, we
compare the sets of labels assigned to all objects and propose to unify
those that have more than 60% of identical labels.
For transparency, the identified candidates are logged for each user,
and a user can also indicate them explicitly instead of relying on
automation.
We conclude that this allows considerably decreasing the complexity of
types and makes the output less redundant.

Future work
===========

In the present paper, we only discuss typing of tree-like values.
However, it is natural to scale this approach to multiple types in APIs,
in which different types are referred to by name and possibly contain
each other.
To address these cases, we plan to show show that the environment of `Typelike` objects
is also `Typelike`, and that constraint generalization (_anti-unification_)
can be extended in the same way. 

It should be noted that many `Typelike` instances for non-simple types usually
follow one the two patterns of (1) for finite sum of disjoint constructors,
we bin this information by each constructor during the inference (2) for typing terms with multiple alternative
representations, we infer all constraints separately for each alternative representation.
In both cases, `Generic` derivation procedure for the `Monoid`, `Typelike`, and `TypeCost` instances is possible[@generics,@generic-monoid].
This allows us to design a type system by declaring datatypes themselves, and leave implementation to the compiler.
Manual implementation would be only left for special cases, like `StringConstraint` and `Counted` constraint.

Finally we believe that we can explain duality of categorical framework of `Typelike` categories
and use unification instead of generalization (anti-unification) as a type inference mechanism. The `beyond` set would then correspond
to a set of error messages, and a result of the inference would represent a principal type in Damas-Milner sense.

Conclusion
----------

In the present study, we aimed to derive the types that were valid with
respect to the provided specification, thereby obtaining the information
from the input in most comprehensive way.
We defined type inference as representation learning and type system
engineering as a meta-learning problem in which the **priors
corresponding to the data structure induced typing rules**.

We also formulated the **union type discipline** as
manipulation of `Typelike` commutative monoids, that
represented knowledge about the data structure.
In addition, we proposed a union type system engineering methodology
that was logically justified by a theoretical criteria. We demonstrated that it
was capable of consistently explaining the decisions made in practice.
We followed a strictly constructive procedure,
that can be implemented generically.

We consider that this kind of *formally justified type system
engineering* can become widely used in practice, replacing *ad-hoc*
approaches in the future.
The proposed approach may be used to underlie the way towards formal
construction and derivation of type systems based on the specification
of value domains and design constraints.

Bibliography {#bibliography .unnumbered}
============

::: {#refs}
:::

# Appendix: definition module headers {#appendix-module-headers .unnumbered}

```{.haskell language="Haskell" file=src/Unions.hs}
{-# language AllowAmbiguousTypes        #-}
{-# language DeriveGeneric              #-}
{-# language DuplicateRecordFields      #-}
{-# language FlexibleInstances          #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language MultiParamTypeClasses      #-}
{-# language NamedFieldPuns             #-}
{-# language PartialTypeSignatures      #-}
{-# language ScopedTypeVariables        #-}
{-# language TypeOperators              #-}
{-# language RoleAnnotations            #-}
{-# language ViewPatterns               #-}
{-# language RecordWildCards            #-}
{-# language OverloadedStrings          #-}
{-# options_ghc -Wno-orphans            #-}
module Unions where

import           Control.Arrow(second)
import           Data.Aeson
import           Data.Maybe(isJust,catMaybes)
import qualified Data.Foldable as Foldable
import           Data.Function(on)
import           Data.Text(Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding  as Text
import qualified Text.Email.Validate(isValid)
import qualified Data.Set  as Set
import           Data.Set(Set)
import           Data.Scientific
import           Data.String
import qualified Data.HashMap.Strict as Map
import           Data.HashMap.Strict(HashMap)
import           GHC.Generics       (Generic)
import           Data.Hashable
import           Data.Typeable
import           Data.Time.Format   (iso8601DateFormat,parseTimeM,defaultTimeLocale)
import           Data.Time.Calendar (Day)
import           Missing

<<freetype>>
<<typelike>>
<<basic-constraints>>
<<row-constraint>>
<<array-constraint>>
<<object-constraint>>
<<presence-absence-constraints>>
<<union-type-instance>>
<<type>>
<<counted>>
<<typecost>>
<<representation>>
```

# Appendix: test suite {.unnumbered}

```{.haskell file=test/spec/Spec.hs}
{-# language FlexibleInstances     #-}
{-# language Rank2Types            #-}
{-# language MultiParamTypeClasses #-}
{-# language MultiWayIf            #-}
{-# language NamedFieldPuns        #-}
{-# language ScopedTypeVariables   #-}
{-# language StandaloneDeriving    #-}
{-# language TemplateHaskell       #-}
{-# language TypeOperators         #-}
{-# language TypeApplications      #-}
{-# language TupleSections         #-}
{-# language UndecidableInstances  #-}
{-# language AllowAmbiguousTypes   #-}
{-# language OverloadedStrings     #-}
{-# language ViewPatterns          #-}
{-# options_ghc -Wno-orphans       #-}
module Main where

import qualified Data.Set            as Set
import qualified Data.Text           as Text
import qualified Data.ByteString.Char8 as BS
import           Control.Monad(when, replicateM)
import           Control.Exception(assert)
import           Data.FileEmbed
import           Data.Maybe
import           Data.Scientific
import           Data.Aeson
import           Data.Proxy
import           Data.Typeable
import qualified Data.HashMap.Strict as Map
import           Data.HashMap.Strict(HashMap)
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Test.Validity.Shrinking.Property
import           Test.Validity.Utils(nameOf)
import qualified GHC.Generics as Generic
import           Test.QuickCheck.Classes
import           System.Exit(exitFailure)

import Test.Arbitrary
import Test.LessArbitrary as LessArbitrary
import Unions

instance Arbitrary Value where
  arbitrary = fasterArbitrary

instance LessArbitrary Value where
  lessArbitrary = cheap $$$? genericLessArbitrary
    where
      cheap = LessArbitrary.oneof [
                pure       Null
              , Bool   <$> lessArbitrary
              , Number <$> lessArbitrary
              ]

instance LessArbitrary          a
      => LessArbitrary (Counted a) where

instance LessArbitrary a
      => Arbitrary     (Counted a) where
  arbitrary = fasterArbitrary

instance Arbitrary Object where
  arbitrary = fasterArbitrary

instance Arbitrary Array where
  arbitrary = fasterArbitrary

class Typelike        ty
   => ArbitraryBeyond ty where
  arbitraryBeyond :: CostGen ty

instance ArbitraryBeyond (PresenceConstraint a) where
  arbitraryBeyond = pure Present

instance ArbitraryBeyond StringConstraint where
  arbitraryBeyond = pure SCAny

instance ArbitraryBeyond IntConstraint where
  arbitraryBeyond = pure IntAny

instance ArbitraryBeyond NumberConstraint where
  arbitraryBeyond = pure NCFloat

instance ArbitraryBeyond RowConstraint where
  arbitraryBeyond = pure RowTop

instance ArbitraryBeyond RecordConstraint where
  arbitraryBeyond = pure RCTop

instance ArbitraryBeyond MappingConstraint where
  arbitraryBeyond =
    MappingConstraint <$$$> arbitraryBeyond
                       <*>  arbitraryBeyond

instance (Ord                      a
         ,Show                     a
         )
      => ArbitraryBeyond (FreeType a) where
  arbitraryBeyond = pure Full

instance ArbitraryBeyond ObjectConstraint where
  arbitraryBeyond = do
    ObjectConstraint <$$$> arbitraryBeyond
                      <*>  arbitraryBeyond

instance ArbitraryBeyond ArrayConstraint where
  arbitraryBeyond = do
    ArrayConstraint <$$$> arbitraryBeyond
                     <*>  arbitraryBeyond

instance ArbitraryBeyond UnionType where
  arbitraryBeyond =
    UnionType       <$$$> arbitraryBeyond
                     <*>  arbitraryBeyond
                     <*>  arbitraryBeyond
                     <*>  arbitraryBeyond
                     <*>  arbitraryBeyond
                     <*>  arbitraryBeyond

instance ArbitraryBeyond          a
      => ArbitraryBeyond (Counted a) where
  arbitraryBeyond = Counted <$> LessArbitrary.choose (0, 10000)
                            <*> arbitraryBeyond

arbitraryBeyondSpec :: forall          ty.
                      (ArbitraryBeyond ty
                      ,Typelike        ty)
                    => Spec
arbitraryBeyondSpec =
  prop "arbitrarybeyond returns terms beyond" $
    (beyond <$> (arbitraryBeyond :: CostGen ty))

instance LessArbitrary Text.Text where
  lessArbitrary = Text.pack <$> lessArbitrary

instance Arbitrary Text.Text where
  arbitrary = Text.pack <$> arbitrary

instance Arbitrary Scientific where
  arbitrary = scientific <$> arbitrary
                         <*> arbitrary

instance (LessArbitrary           a
         ,Ord                     a)
      =>  LessArbitrary (FreeType a) where

instance Arbitrary (FreeType Value) where
  arbitrary = fasterArbitrary
  {-shrink  Full           = []
  shrink (FreeType elts) = map FreeType
                         $ shrink elts-}

instance (Ord               v
         ,Show              v)
      => TypeCost (FreeType v) where
  typeCost  Full        = inf
  typeCost (FreeType s) = TyCost $ Set.size s

{-
instance (Eq                      a
         ,Ord                     a
         ,GenUnchecked            a
         ,LessArbitrary           a
         ,LessArbitrary (FreeType a)
         ,Arbitrary     (FreeType a))
      =>  GenUnchecked  (FreeType a) where
  genUnchecked    = fasterArbitrary
  shrinkUnchecked Full                  = []
  shrinkUnchecked FreeType { captured } =
       map (FreeType . Set.fromList)
     $ shrinkUnchecked
     $ Set.toList captured

instance Validity (FreeType a) where
  validate _ = validate True
 -}

instance LessArbitrary (PresenceConstraint a) where
  lessArbitrary = genericLessArbitraryMonoid
instance Arbitrary     (PresenceConstraint a) where
  arbitrary = fasterArbitrary

instance LessArbitrary IntConstraint where
  lessArbitrary = genericLessArbitraryMonoid
instance Arbitrary     IntConstraint where
  arbitrary = fasterArbitrary

instance LessArbitrary NumberConstraint where
  lessArbitrary = genericLessArbitrary

instance Arbitrary     NumberConstraint where
  arbitrary = fasterArbitrary

listUpToTen :: LessArbitrary a
            => CostGen      [a]
listUpToTen  = do
  len <- LessArbitrary.choose (0,10)
  replicateM len lessArbitrary

instance LessArbitrary StringConstraint where
  lessArbitrary = LessArbitrary.oneof             simple
             $$$? LessArbitrary.oneof (complex <> simple)
    where
      simple  =  pure <$> [SCDate, SCEmail, SCNever, SCAny]
      complex = [SCEnum . Set.fromList <$> listUpToTen]
             <>  simple

instance Arbitrary     StringConstraint where
  arbitrary = fasterArbitrary

instance LessArbitrary ObjectConstraint where
  lessArbitrary = genericLessArbitraryMonoid
instance Arbitrary     ObjectConstraint where
  arbitrary = fasterArbitrary

instance LessArbitrary RecordConstraint where
  lessArbitrary = genericLessArbitraryMonoid
instance Arbitrary     RecordConstraint where
  arbitrary = fasterArbitrary

instance LessArbitrary ArrayConstraint where
  lessArbitrary = genericLessArbitraryMonoid
instance Arbitrary     ArrayConstraint where
  arbitrary = fasterArbitrary

instance LessArbitrary RowConstraint where
  lessArbitrary = genericLessArbitraryMonoid

instance Arbitrary     RowConstraint where
  arbitrary = fasterArbitrary

instance LessArbitrary MappingConstraint where
  lessArbitrary = genericLessArbitraryMonoid
instance Arbitrary     MappingConstraint where
  arbitrary = fasterArbitrary

instance LessArbitrary UnionType where
  lessArbitrary = genericLessArbitraryMonoid

instance Arbitrary     UnionType where
  arbitrary = fasterArbitrary

{-
instance GenUnchecked UnionType where
  genUnchecked    = arbitrary
  shrinkUnchecked = shrink
 -}

{-
instance Validity UnionType where
  validate _ = validate True-}

shrinkSpec :: forall    a.
             (Arbitrary a
             ,Typeable  a
             ,Show      a
             ,Eq        a
             )
           => Spec
shrinkSpec = prop ("shrink on " <> nameOf @a)
           $ doesNotShrinkToItself arbitrary (shrink :: a -> [a])

allSpec :: forall         ty v.
           (Typeable        ty
            ,Arbitrary       ty
            ,Show            ty
            ,Types           ty v
            ,ArbitraryBeyond ty
            ,Arbitrary          v
            ,Show               v
            ) => Spec
allSpec = describe (nameOf @ty) $ do
  arbitraryBeyondSpec @ty
  shrinkSpec    @ty

<<typelike-spec>>
<<types-spec>>
<<typecost-laws>>

-- * Unit tests for faster checking
-- | Bug in generation of SCEnum
scEnumExample = label "SCEnum" $ s == s <> s
  where
    s = SCEnum $ Set.fromList $ [""] <> [Text.pack $ show i | i <- [0..8]]

-- | Bug in treatment of missing keys
objectExample = do
    quickCheck $ label "non-empty object" $ t `check` ob2
    quickCheck $ label "empty object"     $ t `check` ob
  where
    ob  :: Object = Map.fromList []
    ob2 :: Object = Map.fromList [("a", String "b")]
    t   :: ObjectConstraint = infer ob2 <> infer ob

-- | Checking for problems with set.
freetypeExample = label "freetype" $ a <> b == b <> a
  where
    a = FreeType {captured = Set.fromList [Bool False,Bool True,Number (-3000.0),Number 0.6,Number (-1.1e11),Number (-9.0e7),Null]}
    b = FreeType {captured = Set.fromList [Bool False,Bool True,Number  5.0e-6,Null,String "?",Number 1.1e9,Number 3.0e10]}

-- * Run all tests
main :: IO ()
main  = do
  {-
  sample $ arbitrary @Value
  sample $ arbitrary @NullConstraint
  sample $ arbitrary @NumberConstraint
  sample $ arbitrary @RowConstraint
  sample $ arbitrary @RecordConstraint
  sample $ arbitrary @ArrayConstraint
  sample $ arbitrary @MappingConstraint
  sample $ arbitrary @ObjectConstraint
  -}
  quickCheck scEnumExample
  objectExample
  quickCheck freetypeExample

  lawsCheckMany 
    [typesSpec (Proxy :: Proxy (FreeType Value) )
               (Proxy :: Proxy Value     ) True
    ,typesSpec (Proxy :: Proxy NumberConstraint )
               (Proxy :: Proxy Scientific) True
    ,typesSpec (Proxy :: Proxy StringConstraint )
               (Proxy :: Proxy Text.Text ) True
    ,typesSpec (Proxy :: Proxy BoolConstraint   )
               (Proxy :: Proxy Bool      ) True
    ,typesSpec (Proxy :: Proxy NullConstraint   )
               (Proxy :: Proxy ()        ) True
    ,typesSpec (Proxy :: Proxy RowConstraint    )
               (Proxy :: Proxy Array     ) True
    ,typesSpec (Proxy :: Proxy ArrayConstraint  )
               (Proxy :: Proxy Array     ) True
    ,typesSpec (Proxy :: Proxy MappingConstraint)
               (Proxy :: Proxy Object    ) True
    ,typesSpec (Proxy :: Proxy RecordConstraint )
               (Proxy :: Proxy Object    ) True
    ,typesSpec (Proxy :: Proxy ObjectConstraint )
               (Proxy :: Proxy Object    ) True
    ,typesSpec (Proxy :: Proxy UnionType        )
               (Proxy :: Proxy Value     ) True
    ,typesSpec (Proxy :: Proxy (Counted NumberConstraint))
               (Proxy :: Proxy Scientific     ) False
    ]
  representationSpec

typesSpec :: (Typeable  ty
             ,Typeable     term
             ,Monoid    ty
             ,ArbitraryBeyond ty
             ,Arbitrary ty
             ,Arbitrary    term
             ,Show      ty
             ,Show         term
             ,Eq        ty
             ,Eq           term
             ,Typelike  ty
             ,Types     ty term
             ,TypeCost  ty
             )
          =>  Proxy     ty
          ->  Proxy        term
          ->  Bool -- idempotent?
          -> (String, [Laws])
typesSpec (tyProxy   :: Proxy ty)
          (termProxy :: Proxy term) isIdem =
  (nameOf @ty <> " types " <> nameOf @term, [
      arbitraryLaws         tyProxy
    , eqLaws                tyProxy
    , monoidLaws            tyProxy
    , commutativeMonoidLaws tyProxy
    , typeCostLaws          tyProxy
    , typelikeLaws          tyProxy
    , arbitraryLaws                 termProxy
    , eqLaws                        termProxy
    , typesLaws             tyProxy termProxy 
    ]<>idem)
  where
    idem | isIdem    = [idempotentSemigroupLaws tyProxy]
         | otherwise = []

typesLaws :: (          ty `Types` term
             ,Arbitrary ty
             ,ArbitraryBeyond ty
             ,Arbitrary            term
             ,Show      ty
             ,Show                 term
             )
          => Proxy ty
          -> Proxy term
          -> Laws
typesLaws (_ :: Proxy ty) (_ :: Proxy term) =
  Laws "Types" [("mempty contains no terms"
                ,property $
                  mempty_contains_no_terms        @ty @term)
               ,("beyond contains all terms"
                ,property $
                  beyond_contains_all_terms       @ty @term)
               ,("fusion keeps terms"
                ,property $
                  fusion_keeps_terms              @ty @term)
               ,("inferred type contains its term"
                ,property $
                  inferred_type_contains_its_term @ty @term)
               ]

<<representation-examples>>

representationTest :: String -> [Value] -> HType -> IO Bool
representationTest name values repr = do
    if foundRepr == repr
       then do
         putStrLn $ "*** Representation test " <> name <> " succeeded."
         return True
       else do
         putStrLn $ "*** Representation test " <> name <> " failed: "
         putStrLn $ "Values        : " <> show values
         putStrLn $ "Inferred type : " <> show inferredType
         putStrLn $ "Representation: " <> show foundRepr
         putStrLn $ "Expected      : " <> show repr
         return False
  where
    foundRepr :: HType
    foundRepr = toHType inferredType
    inferredType :: UnionType
    inferredType = foldMap infer values

readJSON :: HasCallStack
         => BS.ByteString -> Value
readJSON = fromMaybe ("Error reading JSON file")
         . decodeStrict
         . BS.unlines
         . filter notComment
         . 
         BS.lines
  where
    notComment (BS.isPrefixOf "//" -> True) = False
    notComment  _                           = True

representationSpec :: IO ()
representationSpec  = do
  b <- sequence
    [representationTest "1a" example1a_values example1a_repr
    ,representationTest "1b" example1b_values example1b_repr
    ,representationTest "1c" example1c_values example1c_repr
    ,representationTest "2"  example2_values  example2_repr
    ,representationTest "3"  example3_values  example3_repr
    ,representationTest "4"  example4_values  example4_repr
    ,representationTest "5"  example5_values  example5_repr
    ,representationTest "6"  example6_values  example6_repr]
  when (not $ and b) $
    exitFailure
```

# Appendix: package dependencies {#appendix-package-dependencies .unnumbered}

```{.yaml .hpack file=package.yaml}
name: union-types
version: '0.1.0.0'
category: Web
author: Anonymous
maintainer: example@example.com
license: BSD-3
extra-source-files:
- CHANGELOG.md
- README.md
dependencies:
- base
- aeson
- containers
- text
- hspec
- QuickCheck
- unordered-containers
- scientific
- hspec
- QuickCheck
- validity
- vector
- unordered-containers
- scientific
- genvalidity
- genvalidity-hspec
- genvalidity-property
- time
- email-validate
- generic-arbitrary
- mtl
- hashable
library:
  source-dirs: src
  exposed-modules:
  - Unions
tests:
  spec:
    main: Spec.hs
    source-dirs:
      - test/lib
      - test/spec
    dependencies:
      - union-types
      - mtl
      - random
      - transformers
      - hashable
      - quickcheck-classes
      - file-embed
      - bytestring
  less-arbitrary:
    main: LessArbitrary.hs
    source-dirs:
      - test/lib
      - test/less
    dependencies:
      - union-types
      - mtl
      - random
      - transformers
      - hashable
      - quickcheck-classes
      - quickcheck-instances
```

Appendix: representation of generated Haskell types {.unnumbered}
======================================

We will not delve here into identifier conversion
between JSON and Haskell, so it suffices that we
have an abstract datatypes for Haskell type and constructor identifiers:

```{.haskell #representation}
newtype HConsId  = HConsId String
  deriving (Eq,Ord,Show,Generic,IsString)
newtype HFieldId = HFieldId String
  deriving (Eq,Ord,Show,Generic,IsString)
newtype HTypeId  = HTypeId String
  deriving (Eq,Ord,Show,Generic,IsString)
```

For each single type we will either describe its exact representation or
reference to the other definition by name:
```{.haskell #representation}
data HType =
    HRef HTypeId
  | HApp HTypeId [HType]
  | HADT [HCons]
  deriving (Eq, Ord, Show, Generic)
```

For syntactic convenience, we will allow string literals
to denote type references:
```{.haskell #representation}
instance IsString HType where
  fromString = HRef . fromString
```

When we define a single constructor,
we allow field and constructor names to be empty strings (`""`),
assuming that the relevant identifiers will be put there
by post-processing that will pick names using types of fields
and their containers[@xml-typelift].
```{.haskell #representation}
data HCons = HCons {
              name ::  HConsId
            , args :: [(HFieldId, HType)]
            }
  deriving (Eq, Ord, Show, Generic)
```

At some stage we want to split representation into individually named declarations,
and then we use environment of defined types, with an explicitly named
toplevel type:
```{.haskell #representation}
data HTypeEnv = HTypeEnv {
    toplevel :: HTypeId
  , env      :: HashMap HTypeId HType
  }
```

When checking for validity of types and type environments,
we might need a list of predefined identifiers that are imported:
```{.haskell #representation}
predefinedHTypes :: [HType]
predefinedHTypes = [
    "Data.Aeson.Value"
  , "()"
  , "Double"
  , "String"
  , "Int"
  , "Date" -- actually: "Data.Time.CalendarDay"
  , "Email" -- actually: "Data.Email"
  ]
```

Consider that we also have an `htop` value that represents any possible JSON value.
It is polimorphic for ease of use:

```{.haskell #representation}
htop :: IsString s => s
htop = "Data.Aeson.Value"
```


Code for selecting representation
---------------------------------
Below is the code to select Haskell type representation.
To convert union type discipline to strict Haskell type representations,
we need to join the options to get the actual representation:

```{.haskell #representation}
toHType :: ToHType ty => ty -> HType
toHType  = joinAlts . toHTypes

joinAlts     :: [HType] -> HType
joinAlts []   =  htop -- promotion of empty type
joinAlts alts =  foldr1 joinPair alts
  where
    joinPair a b = HApp ":|:" [a, b]
```

Considering the assembly of `UnionType`, we join all the options,
and convert nullable types to `Maybe` types
```{.haskell #representation}
instance ToHType UnionType where
  toHTypes UnionType {..} =
      prependNullable unionNull opts
    where
      opts = concat [toHTypes unionBool
                    ,toHTypes unionStr
                    ,toHTypes unionNum
                    ,toHTypes unionArr
                    ,toHTypes unionObj]

prependNullable :: PresenceConstraint a -> [HType] -> [HType]
prependNullable Present tys = [HApp "Maybe" [joinAlts tys]]
prependNullable Absent  tys =                tys
```

The type class returns a list of mutually exclusive type representations:

```{.haskell #representation}
class Typelike ty
   => ToHType  ty where
  toHTypes :: ty -> [HType]
```

Conversion of flat types is quite straightforward:
```{.haskell #representation}
instance ToHType BoolConstraint where
  toHTypes Absent  = []
  toHTypes Present = ["Bool"]
instance ToHType NumberConstraint where
  toHTypes NCNever = []
  toHTypes NCFloat = ["Double"]
  toHTypes NCInt   = ["Int"]
instance ToHType StringConstraint where
  toHTypes  SCAny      = ["String"]
  toHTypes  SCEmail    = ["Email"]
  toHTypes  SCDate     = ["Date"]
  toHTypes (SCEnum es) = [HADT $
                          mkCons <$> Set.toList es
                         ]
    where
      mkCons = (`HCons` [])
             .   HConsId
             .   Text.unpack
  toHTypes  SCNever    = []
```

For array and object types we pick the representation which presents the lowest cost of optionality:
```{.haskell #representation}
instance ToHType ObjectConstraint where
  toHTypes ObjectNever           = []
  toHTypes ObjectConstraint {..} =
    if typeCost recordCase <= typeCost mappingCase
      then toHTypes recordCase
      else toHTypes mappingCase

instance ToHType RecordConstraint where
  toHTypes  RCBottom = []
  toHTypes  RCTop    = [htop] -- should never happen
  toHTypes (RecordConstraint fields) =
      [HADT
          [HCons "" $ fmap convert $ Map.toList fields]
      ]
    where
      convert (k,v) = (HFieldId $ Text.unpack k
                      ,toHType v)

instance ToHType MappingConstraint where
  toHTypes MappingNever = []
  toHTypes MappingConstraint {..} =
    [HApp "Map" [toHType keyConstraint
                ,toHType valueConstraint
                ]]

instance ToHType RowConstraint where
  toHTypes  RowNever  = []
  toHTypes  RowTop    = [htop]
  toHTypes (Row cols) =
    [HADT
        [HCons "" $ fmap (\ut -> ("", toHType ut)) cols]
    ]

instance ToHType ArrayConstraint where
  toHTypes ArrayNever           = []
  toHTypes ArrayConstraint {..} =
    if   typeCost arrayCase <= typeCost rowCase
      -- || count <= 3
      then [toHType arrayCase]
      else [toHType rowCase  ]
```

Appendix: Missing pieces of code {#appendix-missing-pieces-of-code .unnumbered}
================================

In order to represent `FreeType` for the `Value`,
we need to add `Ord` instance for it:

```{.haskell #missing}
deriving instance Ord Value
```

For validation of dates and emails, we import functions from Hackage:

```{.haskell #missing}
isValidDate :: Text -> Bool
isValidDate = isJust
            . parseDate
            . Text.unpack
  where
    parseDate :: String -> Maybe Day
    parseDate  = parseTimeM True
                            defaultTimeLocale $
                            iso8601DateFormat Nothing

isValidEmail :: Text -> Bool
isValidEmail = Text.Email.Validate.isValid
           . Text.encodeUtf8
```

```{#not-missing .haskell}
instance (Hashable          k
         ,Hashable            v)
      =>  Hashable (HashMap k v) where
  hashWithSalt s = hashWithSalt s
                 . Foldable.toList

instance Hashable           v
      => Hashable (V.Vector v) where
  hashWithSalt s = hashWithSalt s
                 . Foldable.toList

-- instance Hashable Scientific where
-- instance Hashable Value where
```

Then we put all the missing code in the module:

```{.haskell language="Haskell" file=src/Missing.hs}
{-# language AllowAmbiguousTypes        #-}
{-# language DeriveGeneric              #-}
{-# language DuplicateRecordFields      #-}
{-# language FlexibleInstances          #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language MultiParamTypeClasses      #-}
{-# language NamedFieldPuns             #-}
{-# language PartialTypeSignatures      #-}
{-# language ScopedTypeVariables        #-}
{-# language StandaloneDeriving         #-}
{-# language TypeOperators              #-}
{-# language RoleAnnotations            #-}
{-# language ViewPatterns               #-}
{-# language RecordWildCards            #-}
{-# language OverloadedStrings          #-}
{-# options_ghc -Wno-orphans            #-}
module Missing where

import           Control.Arrow(second)
import           Data.Aeson
import           Data.Maybe(isJust,catMaybes)
import qualified Data.Foldable as Foldable
import           Data.Function(on)
import           Data.Text(Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding  as Text
import qualified Text.Email.Validate(isValid)
import qualified Data.Set  as Set
import           Data.Set(Set)
import           Data.Scientific
import           Data.String
import qualified Data.Vector         as V
import qualified Data.HashMap.Strict as Map
import           Data.HashMap.Strict(HashMap)
import           GHC.Generics       (Generic)
import           Data.Hashable
import           Data.Typeable
import           Data.Time.Format   (iso8601DateFormat,parseTimeM,defaultTimeLocale)
import           Data.Time.Calendar (Day)

<<missing>>
```

[^4]: Compiler feature of checking for unmatched cases.

[^7]: In this case: `beyond (Error _) = True | otherwise = False`.

[^8]: May sound similar until we consider adding more information to the
    type.

[^9]: It should be noted that many but not all type constraints are
    semilattice. Please refer to the counting example below.

[^10]: So both for ∀`a(<> a)` and ∀`a.(a<>)`,
       the result is kept in the `beyond` set.

[^11]: The shortest one according to the information complexity
       principle.

[^13]: The choice of representation will be explained later. Here we only
    consider acquiring information about possible values.

[^14]: The question may arise: what is the *union type* without *set
    union*? When the sets are disjoint, we just put the values in different
    bins to enable easier handling.

[^15]: If we detect a pattern too early, we risk to make the types too
    narrow to work with actual API responses.
