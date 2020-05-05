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

## Defining type inference

If inference fails, we can always correct it by adding additional example.

Minimal definition of the typing inference and checking relations

```{.haskell file=refs/Data/Monoid.hs}
class Semigroup ty where
  (<>) :: ty -> ty -> ty

class Semigroup ty
   => Monoid    ty
  where
    mempty :: ty
<<class_laws_below>>
```

We describe laws as QuickCheck[@quickcheck]
properties so that unit testing can detect
obvious violations.

We use `validity-properties` package[@validity]
for common properties:
```{.haskell file=refs/Data/Monoid.hs}
commutativeSemigroupSpec :: forall    ty.
                            Semigroup ty
                         => Spec
commutativeSemigroupSpec = do
  commutative @ty
  associative @ty
```

Neutral element of the `Typelike` monoid,
`mempty` stands for **no information accepted** about possible value
 (no term seen,
  not even a `null`).
  For example an empty array `[]` could be typed
  as a array type with `mempty` as element type.

In the domain of permissive union types,
`beyond` represents **everything permitted**
or fully dynamic value, when we gathered
information that places any possible value inside the type.
At first reading one may be tempted to claim
that `beyond` set should consists of only a single element -- the `top`.

However, since we defined **unification**
operator `<>`, as **information fusion**,
we might have difficulty in assuring
that no information is lost during our unification.

Also strict type systems tend to have more than
one error value -- since it has to keep the
error message, and track of where the error
originated^[In this case: `beyond (Error _) = True | otherwise = False`.].

This firmly places type inference as a **learning problem**,
and allows us to find the common ground
between dynamic and static typing disciplines.

Languages with static type discipline usually
treat `beyond` as a set of error messages,
since value should have
a statically assigned and **narrow** type,
and `mempty` as a fully polymorphic
type `forall a. a`.

Languages with dynamic type discipline will
treat `beyond` as untyped, dynamic value,
and `mempty` again as an unknown polymorphic
value^[May sound similar until we consider adding
more information to the type.].

Note that `Monoid` operation is a union type unification.

Beside standard laws for monoid
with `beyond` set closed to information addition
by `(<>a)` or `(a<>)` for any value of `a`.

Since we state it as **information acquisition**,
we will use a better interface:
```{.haskell #typelike}
class (Monoid   t
      ,Eq       t
      ,Show     t)
   =>  Typelike t where
   beyond :: t -> Bool
```

That is because we want more than a single `beyond`
element. When typing Haskell, we would
like `beyond` to contain error message,
which makes many.^[Note that many, but not all
type constraints will be semilattice. See counting example below.]

Still we find it useful to state which `Typelike`s
instances are semilattices:
```
idempotentSemigroupSpec :: forall    ty.
                           Semigroup ty
                        => Spec
idempotentSemigroupSpec  = do
  prop (nameOf @ty <> "is idempotent semigroup") $
    idempotentSemigroup @ty

idempotentOperation :: forall ty.
                       Semigroup ty
                    => ty -> Bool
idempotentOperation op a = (a `op` a) `shouldBe` a
```
In case of union types, we accept that there
are many elements in the `beyond` set.
Key property of the `beyond` set, is that it
is closed to information acquisition:
```{.haskell #typelike-spec}
typelikeSpec :: forall       ty.
               (Typelike     ty
               ,GenUnchecked ty
               ,Typeable     ty)
             => Spec
typelikeSpec = do
  monoidSpec    @ty
  prop (nameOf  @ty <> " is commutative") $
    commutative @ty (<>)
  prop "beyond set is closed" $
    beyond_is_closed @ty

beyond_is_closed :: forall   ty.
                    Typelike ty
                 => ty -> ty -> Property
beyond_is_closed ty1 ty2 = do
  beyond ty1 ==> beyond (ty1 <> ty2)
```

It is convenient validation when testing a recursive structure of the type.

Note that we abolish semilattice requirement
that was traditionally assumed for
type constraints here[@semilattice].

That is because this requirement is valid
only for strict type constraint inference,
not for a more general type inference as a learning problem.
As we saw in the example @example:row-constraint,
we need non-monotonic inference when dealing
with alternative representations.

### Typing relation

```{.haskell #typelike}
class Typelike ty
   => ty `Types` val where
   infer ::       val -> ty
   check :: ty -> val -> Bool
```

Here:

* `<>` is unification: associative, commutative operation
* `mempty` is neutral element of unification
* `beyond` set is an attractor of `<>` on both sides
^[So both `forall a. (<> a)` and ∀`a.(a<>)`
  are keep result in the `beyond` set.]

In case of union types, where we learn shape of the type
from examples, we can say that:

* `mempty` represents _no information received_
* `beyond` represents sets of typelikes where
   _no more information will change anything_
or _no more information is accepted_
(for the purpose of typing; the extra
 information may be still useful in error messages)
* `<>` represents fusing information
  on the same entity
  from different observations

We call neutral element to be _no information given_
or _no observation_, and maximum to be
_no more information accepted_.

#### Laws of typing

This is important, since we may
want to separate concerns about
valid domain of types/type constraints
versus concerns of the typing of the terms
by these valid types.

Note that this approach is a bit more relaxed than
full lattice subtyping[@subtype-inequalities][@subtyping-lattice],
since it only considers semilattice with unification operation.



## Laws of typing

```{.haskell #types-spec .hidden}
typesSpec :: forall ty         v.
                   (ty `Types` v
                   ,Arbitrary  v
                   ,Show       v
                   ,Arbitrary  ty
                   ,Show       ty
                   ,Typeable   ty)
          => Spec
typesSpec = do
  describe ("Types " <> nameOf @ty) $ do
    prop "mempty contains no terms" $
      mempty_contains_no_terms        @ty @v
    prop "beyond contains all terms" $
      beyond_contains_all_terms       @ty @v
    prop "inferred type always contains its term" $ do
      inferred_type_contains_its_term @ty @v
```
First we note that to describe _no information_,
`mempty` cannot correctly type any term:

```{.haskell #typelike-spec}
mempty_contains_no_terms
  :: forall    ty term.
     (Typelike ty
     ,Types    ty term)
  =>              term
  -> Expectation
mempty_contains_no_terms term =
      check (mempty :: ty) term
        `shouldBe` False
```

It is also important for typing:
all terms are typed successfully by any value `beyond`.
```{.haskell #typelike-spec}
beyond_contains_all_terms ::
     (Types ty    term
     ,Show        term)
  =>        ty -> term
  -> Property
beyond_contains_all_terms ty term =
  beyond ty ==> term `shouldSatisfy` check ty
```

For typing we have additional rule:
type inferred from a term, must always be valid
for the very same term.
``` {.haskell #typelike-spec}
inferred_type_contains_its_term ::
     forall ty         term.
            ty `Types` term
  =>                   term
  -> Bool
inferred_type_contains_its_term term =
  check ((infer:: term -> ty) term) (term :: term)
```

Law asserts that the following diagram commutes:

```{ .dot width=45% height=20% #fig:type-commutes }
digraph type {
  node [shape=box,color=white];
  subgraph g {
    Bool; Type;
    rank=same;
  }
  Value -> Type [xlabel="infer"];
  Type  -> Bool [label="check with value"];
  Value -> Bool [label="const True"];
}
```

Minimal `Typelike` instance would be one
that contains only `mempty` for _no sample data received_,
and `beyond` for _all values permitted_.

Note that these laws are still compatible
with strict, static type discipline:
`beyond` set is a set of type errors then,
and it is a task of a compiler to disallow
any program with terms that type only to `beyond`
as a least upper bound.

## Type engineering principles

Given that we want to infer the type from finite number of samples
we are presented with _learning problem_,
so we need to use _prior_ knowledge of the domain
to generalize when inferring types.

Clearly after seeing `a: false` we can expect that it is sometimes `a: true`.
After seeing `b: 123` we expect that `b: 100` would also be acceptable.
That means that we need our typing to _learn a reasonable general class from few instances._
That defines making a practical type system as inference problem.

Since our goal is to deliver most descriptive^[Shortest, by information complexity principle.]
types, we will assume that we need to abstract a bit from the _free type_ and take on larger
sets whenever it seems justified.

Another principle is that of **correct operation**,
where given operations on types, we try to find a minimal types
that assure correct operation unexpected errors.

Indeed we want to use this theory to infer a type definition from a finite set of examples,
but we also want it to generalize to infinite types.

Aiming for this, we set the rules of type design:

* type should have a finite description
* inference must be contravariant functor with regards to constructors, for `{"a": X, "b": Y}` is types
  by `T x y`,
  then `X :: x` and `Y :: y` must be also a valid typing.

#### Simple type constraints

1. Given a sample of values, we can have a reasonable approximation of expected values:
  - use `String` versus `Int` outright, instead of any JSON `Value`.
  - assuming that we have a set of parsers that are mutually exclusive, we can implement this for `String` values:

#### Constraints on string type

``` {.haskell #basic-constraints}
data StringConstraint =
    SCDate
  | SCEmail
  | SCEnum  (Set Text)
  | SCNever
  | SCAny
  deriving(Eq, Show,Generic)

instance StringConstraint `Types` Text where
  infer (isValidDate  -> True) = SCDate
  infer (isValidEmail -> True) = SCEmail
  infer  value                 = SCEnum $
                      Set.singleton value

  check  SCDate     s = isValidDate  s
  check  SCEmail    s = isValidEmail s
  check (SCEnum vs) s = s `Set.member` vs
  check  SCNever    _ = False
  check  SCAny      _ = True
```

Then whenever unifying the `String` constraint:
```{.haskell #basic-constraints}
instance Semigroup StringConstraint where
  SCDate     <> (SCEnum _)     = SCAny
  (SCEnum a) <> (SCEnum b)
    | length a + length b < 10 = SCEnum (a <> b)
  (SCEnum a) <> (SCEnum b)     = SCAny

instance Monoid StringConstraint where
  mappend = (<>)
  mempty  = SCNever

instance Typelike StringConstraint where
  beyond  = (==SCAny)
```

#### Constraints on number type

Analogically we may infer for integer constraints^[Program makes it optional `--infer-int-ranges`.] as:
```{.haskell #basic-constraints}
data IntConstraint = IntRange Int Int
                   | IntNever
                   | IntAny
  deriving (Show, Eq)

instance Semigroup IntConstraint where
  IntAny       <> _            = IntAny
  IntNever     <> a            = a
  a            <> IntNever     = a
  IntRange a b <> IntRange c d =
                  IntRange (min a c) (max b d)

instance Typelike IntConstraint where
  beyond = (==IntAny)

instance Monoid IntConstraint where
  mempty = IntNever
```

JavaScript has one number type that holds both `Float` and `Int`, so JSON inherits that:
```{.haskell #basic-constraints}
data NumberConstraint =
    NCInt
  | NCNever
  | NCFloat
  deriving(Eq,Show,Generic)

instance Typelike NumberConstraint where
  beyond = (==NCFloat)

instance Semigroup NumberConstraint where
  <<standard-rules-number-constraint>>
  NCInt <> NCInt = NCInt

instance NumberConstraint `Types` Scientific where
  infer sci
    | base10Exponent sci >= 0 = NCInt
  infer sci                   = NCFloat
  check NCFloat sci = True
  check NCInt   sci = base10Exponent sci >= 0
  check NCNever sci = False

<<standard-instances-number-constraint>>
```

```{.haskell #standard-rules-number-constraint}
NCFloat <> _       = NCFloat
_       <> NCFloat = NCFloat
NCNever <> _       = NCNever
_       <> NCNever = NCNever
```

```{.haskell .hidden #standard-instances-number-constraint}
instance Monoid NumberConstraint where
  mempty = mempty
```

## Free union type

Now for a term with constructors we can infer "free" type for every term:
For any `T` value type Set T` satisfies our notion of _free type_.
``` { .haskell #freetype }
data FreeType a = FreeType { captured :: Set a }
                | Full
  deriving (Eq, Ord, Show)

instance (Ord a, Eq a) => Semigroup (FreeType a) where
  a <> b = FreeType $ (Set.union `on` captured) a b
instance (Ord a, Eq a) => Monoid (FreeType a) where
  mempty  = FreeType Set.empty

instance (Ord a, Eq a, Show a)
      => Typelike (FreeType a) where
  beyond    = (==Full)

instance (Ord      a
         ,Eq       a
         ,Show     a)
      =>  FreeType a `Types` a where
  infer                    = FreeType . Set.singleton
  check Full         _term = True
  check (FreeType s)  term = term `Set.member` s
```


This definition is sound, and for a finite realm of values, may make a sense.
For a set of inputs^[Conforming to Haskell and JSON syntax, we use list for marking the elements of the set.]:
`["yes", "no", "error"]`, we might reasonably say that type is indeed
a good approximation of C-style enumeration, or Haskell-style ADT without constructor arguments.

What is wrong with this notion of _free type_?
It does not generalize at all to infinite and recursive domains! It only allows the objects from the sample, and nothing more.

## Presence and absence constraint

We call this useful case a _presence or absence constraint_:
```{.haskell #presence-absence-constraint}
data PresenceConstraint a =
    Present
  | Absent
  deriving (Eq, Show)

instance Typelike (PresenceConstraint a) where
  mempty = Absent
  beyond    = Present

instance Monoid (PresenceConstraint a) where
  Absent  <> a       = a
  a       <> Absent  = a
  Present <> Present = Present

instance PresenceConstraint a `Types` a where
  infer _         = Present
  check Present _ = True
  check Absent  _ = False
```

While it does not look immediately useful
in context where we always have at least one value
on input, it is important where we can receive empty
array (and thus no values for the element type.)

### Which of these types we may infer?

#### Presence and absence constraints

After seeing `true` value we also expect
`false`, so we can say that the basic constraint
for a boolean value is its presence or absence.

``` {.haskell #presence-absence-constraints}
type BoolConstraint = PresenceConstraint Bool

type role PresenceConstraint nominal

data PresenceConstraint a =
    Present -- ^ some values seen
  | Absent  -- ^ no information
  deriving (Eq,Show,Generic)

instance Semigroup (PresenceConstraint a) where
  Present <> _       = Present
  _       <> Present = Present
  Absent  <> Absent  = Absent

instance Monoid (PresenceConstraint a) where
  mempty = Absent

instance Typelike (PresenceConstraint a) where
  beyond = (==Present)

instance PresenceConstraint a `Types` a where
  infer _ = Present
  check Absent  _ = False
  check Present _ = True
```
Here we have basic presence constraint for any non-Void type:
(...repetition...)

Note that booleans and `null` values are
both denoted by this trivial `PresenceConstraint`
constraint.

In other words, we only
care about presence or absence of their observation.
That means that constraint for boolean
is the simplest possible.

The same for `null`, since there is only one
`null` value.

``` {.haskell #presence-absence-constraints}
type NullConstraint = PresenceConstraint ()
```

### Selecting basic priors

Now that we defined the type system engineering
as prior selection, let's state some obvious rules
for the typing:

We generalize basic datatypes.

Note that we treat `null` as separate basic types,
that can form union with any other.
Thus `mempty` indicates _no type and no value_.
The `beyond` of our semilattice is the type of any `Value` term.

#### Variants

Variant fields for union types are also simple, we implement them with a cousin of `Either` type
that assumes these types are exclusive:
```haskell
data a :|: b = AltLeft  a
             | AltRight b
  deriving (Show, Eq, Generic)

instance (FromJSON  a
         ,FromJSON        b)
      =>  FromJSON (a :|: b) where
  parseJSON a =  AltLeft  <$> decodeEither
             <|> AltRight <$> decodeEither
```
In other words for `Int :|: String` type we first check if the value is `String`, and if it fails try to parse it as `String`.

Variant records are a bit more complicated, since it is unclear which typing is better:

```{.json .javascript file=test/example_variant1.json}
{"message": "Where can I submit my proposal?",
    "uid" : 1014}
{"error"  : "Authorization failed",
   "code" : 401}
```

```{.haskell file=test/example_variant1.result}
data OurRecord =
  OurRecord { message :: Maybe String
            , error   :: Maybe String
            , code    :: Maybe Int
            , uid     :: Maybe Int }
```

Or maybe:
```{.haskell file=test/example_variant2.result}
data OurRecord2 = Message { message :: String
                          , uid     :: Int }
                | Error   { error   :: String
                          , code    :: Int }
```

The best attempt here, is to rely on our examples being reasonable exhaustive.
That is, we can count how many examples we have for each, and how many out of them
are matching. And then compare it to type complexity (with optionalities being more complex than lack of them.)
In this case latter definition has only one choice (optionality), but we only have two samples to begin with.

With more samples, the pattern emerges:
```{.json file=test/example_variant2.json}
{"error"  : "Authorization failed",
    "code":  401}
{"message": "Where can I submit my proposal?",
    "uid" : 1014}
{"message": "Sent it to HotCRP",
    "uid" :   93}
{"message": "Thanks!",
    "uid" : 1014}
{"error"  : "Missing user",
    "code":  404}
```

### Object constraint

In order to avoid information loss,
constraint for JSON object type should
**simultaneously gather information** about
either representing it as a `Map`, or
a record:
```{.haskell #object-constraint}

data MappingConstraint =
  MappingConstraint {
      keyConstraint   :: StringConstraint
    , valueConstraint :: UnionType
    } deriving (Eq, Show, Generic)

instance Monoid MappingConstraint where
  mempty = MappingConstraint {
      keyConstraint   = mempty
    , valueConstraint = mempty
    }

instance Typelike MappingConstraint where
  beyond MappingConstraint {..} =
       beyond keyConstraint
    && beyond valueConstraint

instance Semigroup   MappingConstraint where
  a <> b = MappingConstraint {
      keyConstraint   =
        ((<>) `on` keyConstraint  ) a b
    , valueConstraint =
        ((<>) `on` valueConstraint) a b
    }

instance MappingConstraint `Types`
         Object where
  infer obj =
    MappingConstraint
      (foldMap infer $ Map.keys obj)
      (foldMap infer            obj)
  check (MappingConstraint {..}) obj =
       all (check keyConstraint)
           (Map.keys obj)
    && all (check valueConstraint)
           (Foldable.toList obj)
```

Separately we gather information about
the possible typing of the JSON object
as a record of values:
```{.haskell #object-constraint}
data RecordConstraint =
    RCTop
  | RCBottom
  | RecordConstraint {
        fields :: HashMap Text UnionType
      } deriving (Show,Eq,Generic)

instance Typelike RecordConstraint where
  beyond = (==RCTop)

instance Semigroup   RecordConstraint where
  RCBottom <> a        = a
  a        <> RCBottom = a
  RCTop    <> _        = RCTop
  _        <> RCTop    = RCTop
  a        <> b        = RecordConstraint $
    Map.unionWith (<>) (fields a)
                       (fields b)

instance Monoid      RecordConstraint where
  mempty = mempty

instance RecordConstraint `Types` Object
  where
    infer =  RecordConstraint
          .  Map.fromList
          .  fmap (second infer)
          .  Map.toList
    check RCTop    _ = True
    check RCBottom _ = False
    check rc obj
            | Map.keys (fields rc)
           == Map.keys        obj  =
      and $ Map.elems $
        Map.intersectionWith  check
                             (fields rc)
                              obj
    check _  _ = False
```

Seeing that the two abstract domains above
are independent, we store information
about both option separately in a record
^[Choice of representation will be explained later.
Here we only consider gathering of information
about the possible values.]

```{.haskell #object-constraint}
data ObjectConstraint = ObjectConstraint {
    mappingCase :: MappingConstraint
  , recordCase  :: RecordConstraint
  } deriving (Eq,Show,Generic)

instance Semigroup ObjectConstraint where
  a <> b =
    ObjectConstraint {
      mappingCase =
        ((<>) `on` mappingCase) a b
    , recordCase =
        ((<>) `on` recordCase ) a b
    }

instance Monoid ObjectConstraint where
  mempty = ObjectConstraint {
             mappingCase = mempty
           , recordCase  = mempty
           }

instance Typelike ObjectConstraint where
  beyond ObjectConstraint {..} =
       beyond mappingCase
    && beyond recordCase

instance ObjectConstraint `Types` Object where
  infer v = ObjectConstraint (infer v)
                             (infer v)
  check ObjectConstraint {..} v =
       check mappingCase v
    && check recordCase  v
```

Note that this representation is similar
in sense to _intersection type_:
any value that satisfies `ObjectConstraint`
must satisfy by contained `mappingCase`,
and `recordCase`.

Note that this _intersection approach_
to conflicting union type constraints enjoys
_principal type property_ -- principal
type simply gathers information from different
angles and handles it separately.

### Array constraint

Similarly to the object,
`ArrayConstraint` should simultaneously gather
information about all possible representations
of the array:

* an array of the same elements
* row with the type depending on the column

We need to gather information for both
alternatives separately, and then
measure relatively likelihood
of either case just before mapping
the union type to Haskell declaration.

Again, we put the record of
two different possible representations:
```{.haskell #array-constraint}

data ArrayConstraint  = ArrayConstraint {
    arrayCase :: UnionType
  , rowCase   :: RowConstraint
  }
  deriving (Show, Eq, Generic)

instance Monoid ArrayConstraint where
  mempty = ArrayConstraint {
             arrayCase = mempty
           , rowCase   = mempty
           }

instance Typelike ArrayConstraint where
  beyond ArrayConstraint {..} =
       beyond arrayCase
    && beyond rowCase

instance Semigroup ArrayConstraint where
  a1 <> a2 =
    ArrayConstraint {
      arrayCase = ((<>) `on` arrayCase) a1 a2
    , rowCase   = ((<>) `on` rowCase  ) a1 a2
    }

<<row-constraint>>

instance ArrayConstraint `Types` Array
  where
    infer vs =
      ArrayConstraint
        (mconcat (infer <$>
               Foldable.toList vs))
        (infer              vs)
    check ArrayConstraint {..} vs =
         and (check arrayCase <$>
                Foldable.toList vs)
      && check rowCase   vs

```

### Row constraint

Row constraint is valid only if
there is a fixed number of entries in each
row, which we represent by escaping to the `beyond`
whenever there is uneven number of columns.

``` {.haskell #row-constraint}
data RowConstraint =
     RowTop
   | RowBottom
   | Row       [UnionType]
   deriving (Eq,Show,Generic)

instance Typelike RowConstraint where
  beyond = (==RowTop)

instance Monoid RowConstraint where
  mempty = RowBottom

instance RowConstraint `Types` Array where
  infer = Row
        . Foldable.toList
        . fmap infer
  check RowTop    _ = True
  check RowBottom _ = False
  check (Row rs) vs
    | length rs == length vs =
      and $
        zipWith check                 rs
                     (Foldable.toList vs)
  check  _        _ = False

instance Semigroup RowConstraint where
  Row bs    <> Row cs
    | length bs /= length cs = RowTop
  Row bs    <> Row cs        =
    Row $ zipWith (<>) bs cs
```

In other words, `RowConstraint` is a _levitated
semilattice_[@levitated-lattice]
with neutral element over content
type `[UnionType]`.

```{.haskell #row-constraint-standard-rules .hidden}
  RowBottom <> r         = r
  r         <> RowBottom = r
  RowTop    <> _         = RowTop
  _         <> RowTop    = RowTop
```

### Putting it together into a union

Since we have a union of just few possible constraints,
we make it a record for easier processing:

Note that given constraints for different type
constructors, union type can be though of
as mostly generic monoid[@generic-monoid]:

```{.haskell #type}
data UnionType =
  UnionType {
    unionNull :: NullConstraint
  , unionBool :: BoolConstraint
  , unionNum  :: NumberConstraint
  , unionStr  :: StringConstraint
  , unionArr  :: ArrayConstraint
  , unionObj  :: ObjectConstraint
  }
  deriving (Eq,Show,Generic)

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
Generic structure of union type can be explained
by the fact that informations contained in different
record fields are _independent from each other_.
That means that we compute the meet over different dimensions.

```{.haskell #type}
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

Since we described `beyond` set that is
either **accepting any value**,
and **accepting no more information**,
its definition should be no surprise:

```{.haskell #type}
instance Typelike UnionType where
  beyond UnionType {..} =
      beyond unionNull
   && beyond unionBool
   && beyond unionNum
   && beyond unionStr
   && beyond unionObj
   && beyond unionArr
```

Inference breaks disjoint alternatives
to different record fields,
depending on the constructor of given value.

This allows as for clear and efficient
treatment of values distinguished by different
dynamic type into different partitions
of the union^[Impatient reader could ask: what is the _union type_ without _set union_? When the sets are disjoint, we just put the values in different bins for easier handling.]

``` {.haskell #union-type-instance}
instance UnionType `Types` Value where
  infer (Bool   b) = mempty { unionBool = infer b  }
  infer  Null      = mempty { unionNull = infer () }
  infer (Number n) = mempty { unionNum  = infer n  }
  infer (String s) = mempty { unionStr  = infer s  }
  infer (Object o) = mempty { unionObj  = infer o  }
  infer (Array  a) = mempty { unionArr  = infer a  }
  check UnionType { unionNum } (Number n) =
              check unionNum           n
  check UnionType { unionStr } (String s) =
              check unionStr           s
  check UnionType { unionObj } (Object o) =
              check unionObj           o
  check UnionType { unionArr } (Array  a) =
              check unionArr           a
```

### Overlapping alternatives

Crux of union type systems have been long
dealing with conflicting types on the input.

Motivated by examples above, we want to also deal
with conflicting alternative assignments.

It is apparent that examples 4. to 6.
hint at more than one assignment:

5. Either a list of lists of values that are one of `Int`, `String`, or `null`, or a table that has the same (and predefined) type
for each

6. Either a record of fixed names,
or the mapping from hash to a single object type.

### Counting observations

How can we make sure that we have a right number of samples?
This is another example:
```json
{"samples":
[{"error"  : "Authorization failed",
  "code"   :  401}
,{"message": "Where can I submit my proposal?",
     "uid" : 1014}
,{"message": "Sent it to HotCRP",
     "uid" :   93}
,{"message": "Thanks!",
     "uid" : 1014}
,{"error"  : "Authorization failed",
     "code":  401}
]}
```
First we need to identify it as a list of same elements,
and then to notice, that there are multiple instances of each record example.
That suggests that the best would be to use not sets, but multisets
of inferred records, and attempt to minimize the term.

Next is detection of similarities between type descriptions developed
for different parts of the term:
```json
{"samples"      :  [...],
 "last_message" : {"message": "Thanks!",
                      "uid" : 1014}
}
```
We can add auxiliary information about number of samples seen
and the constraint will stay `Typelike`:

```{.haskell #counted }
data Counted a =
  Counted { count      :: Int
          , constraint :: a
          } deriving (Eq, Show)

instance Semigroup          a
      => Semigroup (Counted a) where
  a <> b = Counted (count      a +  count      b)
                   (constraint a <> constraint b)

instance Monoid  a
      => Monoid (Counted a) where
  mempty = Counted 0 mempty

instance Typelike          a
      => Typelike (Counted a) where
  beyond Counted {..} = beyond constraint
```

We can connect `Counted` as parametric functor
to our types in order to track auxiliary
information.

Note that the `Counted` constraint is the
first example of constraint that is not
a semilattice, that is `a<>a/=a`.

This is because it is `Typelike`, but
it is not a type constraint in a traditional sense,
instead it counts the samples observed for the
constraint inside, so we can decide
which alternative representation is best supported
by evidence.

Thus at each step we might want to keep a **cardinality** of each possible value,
and given enough samples, attempt to detect patterns ^[If we detect pattern to early, we risk make our types to narrow to work with actual API answers.].

In order to preserve efficiency, we might want to merge whenever, number of alternatives in the multiset crosses the threshold.
^[Option `--max-alternative-constructors=N`]
And only attempt to narrow strings when cardinality crosses the threshold ^[Option `--min-enumeration-cardinality`.]

# Choosing representation

## Heuristics for better types

Final touch would be to postprocess assigned type
before generating it, in order to make it more resilient
to common uncertainties.

Note that these assumptions my sidestep
our validity criterion from initial part
of the paper, however they proved to work
well in practice.

### Array type with no element observations

If we have no observations of array type,
it can be inconvenient to disallow array to
contain any value at all.
Thus we make a non-monotonic step of
converting final `mempty` to representation
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
and user can also indicate them explicitly instead
of relying on automation.

We found that this greatly decreases complexity of the types,
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
  during `infer`ence
2. for typing terms that have two alternative
  representations we apply
  we `infer` all constraints separately,
  by applying `infer`ence to the same term

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
from specification of value domains
and design constraints.

# Bibliography {.unnumbered}

::::: {#refs}

:::::



# Appendix: module headers {.unnumbered}

```{.haskell file=src/Unions.hs .hidden}
{-# language AllowAmbiguousTypes    #-}
{-# language DeriveGeneric          #-}
{-# language DuplicateRecordFields  #-}
{-# language FlexibleInstances      #-}
{-# language FunctionalDependencies #-}
{-# language MultiParamTypeClasses  #-}
{-# language NamedFieldPuns         #-}
{-# language PartialTypeSignatures  #-}
{-# language ScopedTypeVariables    #-}
{-# language StandaloneDeriving     #-}
{-# language TypeApplications       #-}
{-# language TypeOperators          #-}
{-# language RoleAnnotations        #-}
{-# language TypeSynonymInstances   #-}
{-# language ViewPatterns           #-}
{-# language RecordWildCards        #-}
module Unions where

import           Control.Arrow(second, (***), first)
import           Data.Aeson
import           Data.Maybe(isJust)
import qualified Data.Foldable as Foldable
import           Data.Time.Calendar(Day)
import           Data.Either
import           Data.Function(on)
import           Data.Proxy
import           Data.Text(Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding  as Text
import qualified Text.Email.Validate(isValid)
import qualified Data.Set  as Set
import           Data.Set(Set)
import           Data.Scientific
import           Data.List(sortBy)
import qualified Data.HashMap.Strict as Map
import           Data.HashMap.Strict(HashMap)
import           GHC.Generics(Generic)
import           Data.Time.ISO8601
import           Data.Time.Clock(UTCTime(utctDay))

<<typeclass>>
<<freetype>>
<<typelike>>
<<basic-constraints>>
<<array-constraint>>
<<object-constraint>>
<<presence-absence-constraints>>
<<union-type-instance>>
<<type>>
<<counted>>

<<missing>>
```

```{.haskell file=test/Spec.hs .hidden}
{-# language FlexibleInstances     #-}
{-# language Rank2Types            #-}
{-# language MultiParamTypeClasses #-}
{-# language ScopedTypeVariables   #-}
{-# language StandaloneDeriving    #-}
{-# language TypeOperators         #-}
{-# language TypeApplications      #-}
{-# language UndecidableInstances  #-}
{-# language AllowAmbiguousTypes   #-}
module Main where

import qualified Data.HashMap.Strict as Map
import qualified Data.Vector         as Vector
import qualified Data.Text           as Text
import qualified Data.Text.Encoding  as Text
import Data.Scientific
import Data.Aeson
import Data.Proxy
import Data.Typeable
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Gen
import Data.Proxy
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.Validity hiding(check)
import Test.Validity.Monoid
import Test.Validity.Utils(nameOf)

import Unions

-- FIXME: use json-autotype's
instance Arbitrary Value where
  arbitrary = oneof [
      pure Null
    , Bool   <$> arbitrary
    , String <$> arbitrary
    , Number <$> arbitrary
    , Array  <$> Vector.fromList <$> listOf arbitrary
    , Object <$> Map.fromList
             <$> listOf ((,) <$> arbitrary
                             <*> arbitrary)
    ]

instance Arbitrary Text.Text where
  arbitrary = Text.pack <$> arbitrary

instance Arbitrary Scientific where
  arbitrary = scientific <$> arbitrary <*> arbitrary

main = hspec spec

spec = do
  describe "Free types" $ do
    typelikeSpec @(FreeType Value)
    typesSpec @(FreeType Value) @Value
  describe "JSON types" $ do
    typelikeSpec @UnionType
    typesSpec    @UnionType @Value

<<typelike-spec>>
<<types-spec>>
```

# Appendix: package dependencies {.unnumbered}

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
- iso8601-time
- time
- email-validate
library:
  source-dirs: src
  exposed-modules:
  - Unions
tests:
  spec:
    main: Spec.hs
    source-dirs: test
    dependencies:
      - union-types
```

# Appendix: Hindley-Milner as `Typelike` {.unnumbered}

# Appendix: Missing pieces of code {.unnumbered}
``` {.haskell #missing .hidden}
-- In order to represent `FreeType` for the `Value`,
-- we need to add `Ord` instance for it:
instance Ord       Value where
  compare = compare `on` encodeConstructors

fromEnum' :: Enum a => a -> Integer
fromEnum' = fromIntegral . fromEnum

encodeConstructors :: Value -> [Integer]
encodeConstructors  Null      = [0]
encodeConstructors (Bool   b) = [1, fromEnum' b]
encodeConstructors (Number n) = [2,
        fromIntegral $ base10Exponent n,
        coefficient n]
encodeConstructors (String s) = 3:
  (fromEnum' <$> Text.unpack s)
encodeConstructors (Array  a) = 4:
  concatMap encodeConstructors a
encodeConstructors (Object o) =
    concatMap encodeItem      $
    sortBy (compare `on` fst) $
    Map.toList o
  where
    encodeItem (k, v) =
      (fromEnum' <$> Text.unpack k) <>
      encodeConstructors v
```

```{.haskell #missing}

isValidDate :: Text -> Bool
isValidDate = isJust
            . fmap utctDay
            . parseISO8601
            . Text.unpack

isValidEmail :: Text -> Bool
isValidEmail = Text.Email.Validate.isValid
           . Text.encodeUtf8
```
