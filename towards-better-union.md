---
title:  "Towards a more perfect union type"
shorttitle: "Towards perfect union type"
author:
  - name: Michał J. Gajda
    email: mjgajda@migamake.com
    affiliation:
      institution: Migamake Pte Ltd
tags:
  - Haskell
  - JSON
  - union type
  - type providers
abstract: |
  We present a principled theoretical
  framework for dealing with union types,
  system, and show its work in practice
  on JSON data structures.

  The framework poses union type
  inference as a problem of learning
  from multiple examples.
  Mathematical framework is quite
  generic, and easily extensible.
date:   2020-04-04
description: |
  Musings on union types
link: "https://gitlab.com/migamake/json-autotype"
bibliography:
  - towards-better-union.bib
csl: acm-template/acm-sig-proceedings.csl
prologue: |
  \let\longtable\tabular
  \let\endlongtable\endtabular
  \renewcommand{\url}[1]{\href{#1}{[Link]}}
link-citations: true
tables: true
listings: true
acks: |

  Author thanks for all tap-on-the-back donations to his past projects.

  The text was prepared with great help of bidirectional literate programming[@literate-programming] tool[@entangled],
  Pandoc[@pandoc] markdown publishing system and live feedback from GHCid[@ghcid].

---

# Introduction

Typing dynamic languages has been long
considered a challenge [@javascript-inference],
but the importance
of the task grows with ubiquity
of JSON cloud APIs with only example
documents in documentation.

Past work have suggested it is possible
to infer decent type mappings
from sample data [@json-autotype-prezi; @quicktype; @type-providers-f-sharp].

We expand on these results,
by presenting the framework
for type systems as learning algorithms,
formulate it mathematically,
and show its performance on
JSON API examples.

Our framework enjoy mathematical
theory, complete typing relation,
and allows to easily add new features.

## Related work

### Union type providers

F# type providers for JSON allow to automatically
derive schema, but the type system is
_ad-hoc_[@type-providers-f-sharp].

There was a previous effort to apply union types
to JSON inference to generate Haskell
types[@json-autotype-prezi],
but it also lacks a rigorous formal treatment.

Another attempt to automatically infer schemas
also in the PADS project [@pads],
but it does not provide a generalized type system
design methodology.

There is a program called [@quicktype] that
tries to derive types with the Markov chains
but its cost requires much more engineering
time since unit tests are case-by-case,
and again there is little underlying theory.

Thus we have precedents of past work
that deliver partially satisfactory results,
that we want to expand in order
to systematically add more and more features.

### Frameworks for expressing type systems

Type systems are commonly expressed
as partial relation of _typing_,
and their properties, like subject reduction
expressed relative to (also partial)
relation of _reduction_ within a term rewriting
system.

There are general formulations
of Hindley-Milner type systems relative parameterized
by constraints [@HM-X,@Jones].

We are not aware of any attempt
to formulate general laws that would apply
to all union type systems. Nor are we aware
of any previous formulation that would
use complete relations or functions
in order to provide consistent
mathematical descriptions of what
happens where terms go beyond
their desired types^[Or at least beyond `bottom`
exploding to _infamous undefined behaviour_[@undefined1,@undefined2,@undefined3].]

# Motivation

## Motivating examples

Now let's give some motivating examples from realm of JSON API types:

1. Subsets of data within a single constructor:
  * _API argument is and email_ - this is subset of valid `String` values, that we can usefully validate on the client.
```{.json file=test/example1a.json .hidden}
{"message": "Where can I submit my proposal?",
    "uid" : 1014}
{"error"  : "Authorization failed",
   "code" : 401}
```
```{.json file=test/example1a.result .hidden}
newtype Example = Example Email
```
  * _Page size determines number of results to return (min: 10, max:10000)_ - this is also a subset of `Int` values between `10`, and `10000`
```{.json file=test/example1b.json .hidden}
{"example": [
  10,
  10000
]}
```
```{.json file=test/example1b.result .hidden}
newtype Example = Example [Int]
```
  * _`date` contains ISO8601 date_ -- contains a `String` in format `"2019-03-03"` but not `String` in format `"The third day of the month of March, Anno Domini 2019"`
```{.json file=test/example1c.json .hidden}
"2019-03-03"
```
```{.json file=test/example1c.result .hidden}
newtype Example = Example Date
```
2. Optional fields:
  * _Page size is 100 by default_ - that is we have `{"page_size": 50}` or `{}`
```{.json file=test/example2.json .hidden}
{}
{"page_size": 50}
```
```{.haskell file=test/example2.result .hidden}
newtype Example = Example { page_size :: Maybe Int }
```
3. Variant fields:
  * _Answer to the query is either a number of of registered objects, or String `"unavailable"`_ - this is `Int` value or a `String`
```{.json file=test/example3.json .hidden}
"alpha"
10
```
```{.haskell file=test/example3.result .hidden}
newtype Example = Example (String :|: Int)
```

4. Variant records:
  * _Answer contains either text message with user id, or an error._ -- That is either:
```{.json file=test/example4.json}
{"message" : "Where can I submit my proposal?", "uid"  : 1014}
{"message" : "Submit it to HotCRP",             "uid"  :  317}
{"error"   : "Authorization failed",            "code" :  401}
{"error"   : "User not found",                  "code" :  404}
```
5. Arrays in place of records^[Which strikes author as a bad practice, but it is part of real-life APIs. We might want to make it optional with `--array-records` option.]:
``` {.json file=test/example5.json}
[
  [1, "Nick",    null       ]
, [2, "George", "2019-04-11"]
, [3, "Olivia", "1984-05-03"]
]
```
```{.haskell file=test/example5.result .hidden}
data Examples = Examples [Example]

data Example = Example {
    field1 :: Int
  , field2 :: String
  , field3 :: Maybe Date
  }
```

6. Maps of identical objects^[Example taken from @quicktype.]:
``` { .json file=test/example6.json }
{
    "6408f5": {
        "size": 969709,
        "height": 510599,
        "difficulty": 866429.732,
        "previous": "54fced"
	},
	"54fced": {
	    "size": 991394,
        "height": 510598,
        "difficulty": 866429.823,
        "previous": "6c9589"
	},
	"6c9589": {
        "size": 990527,
        "height": 510597,
        "difficulty": 866429.931,
        "previous": "51a0cb"
	}
}
```
```{.haskell file=test/example6.result .hidden}
newtype Examples = Example (Map String Example)

data Example {
    size       :: Int
  , height     :: Int
  , difficulty :: Double
  , previous   :: String
  }
```

::::: {#example:nonmonotonic-inference}

Note that the last example above makes
the type inference non-monotonic, since
a dictionary with a single key would
have an incompatible type:

```
data Example = Example { f_6408f5 :: O_6408f5 }
data O_6408f5 = O_6408f5 {
    size       :: Int
  , height     :: Int
  , difficulty :: Double
  , previous   :: String
  }
```

It also suggests that the user might want to
explicitly add evidence for one of alternative
representations in case samples are insufficient.
(Like in case of single element dictionary.)

:::::

## What is the goal of inference?

Given another undocumented (or wrongly labelled) JSON API, we want to read the input
into Haskell, and avoid checking for _unexpected_ deviations of the format.
At the same time, we want to accept all known valid inputs outright,
so that we can use types^[And compiler feature of checking for unmatched cases.]
to make sure we exhaustively treat the input.

Thus we can assume that the smallest non-singleton set is a better approximation type than a singleton.
We call it _minimal containing set principle_.

Second we can prefer types that allow for _less degrees of freedom_ than the others,
while conforming to some commonly occuring structure. We call it _information content principle_.

Given these principles, and examples of frequently occuring patterns,
we can infer a reasonable _world of types_ that would be used as approximations,
instead of making this creation ad-hoc. That is _type system engineering_,
that allows us to derive type system design directly from information on data structures
and likelihood of their occurence.

# Problem definition

## Preliminaries

### JSON values

Since we are interested in JSON, we use Haskell encoding of JSON term for convenient reading^[As used by Aeson[@aeson] package.]:
``` {.haskell file=refs/Data/Aeson.hs}
data Value =
    Object (Map String Value)
  | Array  [Value]
  | String  Text
  | Number  Scientific
  | Bool    Bool
  | Null
```

In order to accomodate both integers
and exact decimal fractions^[JavaScript and JSON use binary floating point instead, but we stick to the representation chosen by `aeson` library that parses JSON.] in our number
representation,
we use decimal floating point[@scientific]:

```{.haskell file=refs/Data/Scientific.hs}
data Scientific =
  Scientific { coefficient    :: Integer
             , base10Exponent :: Int }
```
