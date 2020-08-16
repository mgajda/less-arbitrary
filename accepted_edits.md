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
  In this paper, we present a theoretical framework to deal with union
types and demonstrate its applicability in practice onto JavaScript
object notation (JSON) data structures.

The proposed approach considers union type inference as a problem of
learning from multiple examples. The underlying mathematical framework
is rather generic and easily extensible.

date:   2020-04-04
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

Introduction
============

The usage of dynamically typed programming languages has been considered
as a challenge \[\@javascript-inference\] for a long time. However, the
importance of this question has augmented with the ubiquity of
JavaScript object notation (JSON) cloud application programming
interfaces (APIs), having only a limited number of sample documents
available.

Previous research works have suggested that it is possible to infer
appropriate type mappings from sample data \[\@json-autotype-prezi;
\@quicktype; \@type-providers-f-sharp\].

In the present study, we rely on these results to propose a framework
for type systems in programming languages in a form of learning
algorithms, formulate it mathematically, and evaluate its performance on
JSON API examples.

The proposed framework is grounded on mathematical theory and complete
typing relation. It is intended to add new features easily.

Related work
------------

Union type providers

F\# type providers for JSON facilitate specifying a schema
automatically; however, generally, a type system is *ad-hoc*
\[\@type-providers-f-sharp\].

Previously, the efforts have been made to apply union types to perform
inference in JSON by generating Haskell types \[\@json-autotype-prezi\];
however, this approach lacks a rigorous formularization.

The other attempt to automatically infer schemas has been introduced in
the PADS project \[\@pads\]. Nevertheless, it should be noted that it
has not specified a generalized type-system design methodology.

A program called \[\@quicktype\] has been developed to derive types
based on Markov chains; however, it is associated with additional
computational costs, as it requires considerable engineering time due to
the implementation of unit tests in a case-by-case mode. Moreover, this
approach lacks sound underlying theory.

Therefore, we observe that there are several previously introduced
approaches that provide partially satisfactory results. In the present
study, we aim to expand these proposals to enable the possibility of
adding an increasing number of features systematically.

### Frameworks corresponding to type systems

Type systems are commonly considered as a partial relation of *typing*,
and their properties, such as subject reduction, are also expressed
relatively to the relation (also partial) of *reduction* within a term
rewriting system.

General formulations have been introduced for the Hindley-Milner type
systems relatively parameterized by constraints \[\@HM-X, \[\@Jones\]\].

We are not aware of any attempts to formulate general laws that would
apply to all existing union type systems. Moreover, to the best of our
knowledge, no previous formulation exists that would consider complete
relations or functions, providing consistent mathematical descriptions
for the underlying type system mechanisms beyond predefined types[^1].

Motivation
==========

Motivating examples
-------------------

Here, we consider several examples of JSON API types underlying the
motivation for the present study:

1.  Subsets of data within a single constructor:

-   *API* *argument* -- it is a subset of valid String values that can
    be validated on the client side.

```{=html}
<!-- -->
```
-   *The page size determines the number of results to return (min: 10,
    max: 10,000)* -- it is also a subset of Integer (Int) values between
    10 and 10,000.

```{=html}
<!-- -->
```
-   *The date corresponds to a ISO8601 date* -- a date are represented
    as a String in the format \"2019-03-03\".

2.  Optional fields:

-   *The page size is equal* *to 100* *by default* -- it means that we
    have {"page\_size": 50} or {}.

3.  Variant fields:

-   *Answer to a query is either a number of registered objects or
    String \"unavailable\"* -- this is Int value or a String.

4.  Variant records:

-   *Answer contains either a text message with user id or an error.* --
    This can be represented as one of the following options:

```{=html}
<!-- -->
```
    {"message": "Where can I submit my proposal?", "uid": 1014};
    {"message": "Submit it to HotCRP", "uid": 317};
    {"error"   : "Authorization failed", "code": 401};
    {"error"   : "User not found", "code": 404}.

5.  Arrays corresponding to records[^2]:

```{=html}
<!-- -->
```
    [
      [1, "Nick", null]
    , [2, "George", "2019-04-11"]
    , [3, "Olivia", "1984-05-03"]
    ]

6.  Maps of identical objects[^3]:

```{=html}
<!-- -->
```
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

It should be noted that the last example presented above results in
setting the type inference non-monotonic, as a dictionary with a single
key would have an incompatible type:

    data Example = Example {f_6408f5 :: O_6408f5}
    data O_6408f5 = O_6408f5 {
        size :: Int
      , height :: Int
      , difficulty :: Double
      , previous :: String
      }

It also suggests that a user might decide to explicitly add an evidence
for one of alternative representations in the case when samples are
insufficient (similarly to a single element dictionary).

Goal of inference
-----------------

Given an undocumented (or incorrectly labelled) JSON API, we may need to
read the input of Haskell encoding and avoid checking for the presence
of *unexpected* format deviations. At the same time, we may decide to
accept all known valid inputs outright so that we can use types[^4] to
ensure that the input is processed exhaustively.

Accordingly, we can assume that the smallest non-singleton set is a
better approximation type than a singleton. We refer to this observation
as a *minimal containing set principle*.

Then, we may prefer focusing on types that provide a *fewer number of
degrees of freedom* compared with the others, while conforming to a
commonly occurring structure. We denote it as an *information content
principle*.

Considering these principles and examples of frequently occurring
patterns, we can infer a reasonable *world of types* that can be used as
approximations, instead of establishing this procedure in an ad-hoc
manner. In this way, we can implement *type system engineering* that
allows deriving type system design directly from the information about
data structures and the likelihood of their occurrence.

Problem definition
==================

Preliminaries
-------------

JSON values

As we focus on JSON, we utilize Haskell encoding of the JSON term for
convenient reading[^5] specified as follows:

    data Value =
        Object (Map String Value)
      | Array  [Value]
      | String  Text
      | Number  Scientific
      | Bool    Bool
      | Null

To incorporate both integers and exact decimal fractions[^6] in the
considered number representation, we employ a decimal floating point
\[\@scientific\] defined as follows:

    data Scientific =
      Scientific { coefficient    :: Integer
                 , base10Exponent :: Int }

Defining type inference
-----------------------

If inference fails, it is possible to correct it by introducing an
additional example.

The minimal definition of typing inference and relation check can be
formulated as follows:

    class Semigroup ty where
      (<>) :: ty -> ty -> ty

    class Semigroup ty
       => Monoid ty
      where
        mempty :: ty

We describe the laws as QuickCheck \[\@quickcheck\] properties so that
unit testing can be implemented to detect obvious violations.

We utilize the validity-properties package \[\@validity\] for common
properties as follows:

    commutativeSemigroupSpec :: forall       ty.
                               (Semigroup    ty
                               ,Show         ty
                               ,Eq           ty
                               ,Arbitrary    ty
                               )
                             => Spec
    commutativeSemigroupSpec = do
      prop "commutative" $
            commutativeOnArbitrary @ty (<>)
      prop "associative" $
            associativeOnArbitrary @ty (<>)

A neutral element mempty of a Typelike type class referred to as Monoid
corresponds to the case of **no information accepted** about a possible
value (no term seen, not even a null). For example, an empty array \[\]
can be referred to as an array type with mempty as an element type.

In the domain of permissive union types, a 'beyond' set represents the
case of '**everything permitted'** or fully dynamic value, when we
gather the information that contains any possible value inside a type.
At the first reading, it may be deemed that a 'beyond' set should
comprise only one single element -- the top one.

However, as we define a **unification** operator \<\> as **information
fusion**, we may encounter difficulties in assuring that no information
has been lost during the unification.

Moreover, strict type systems usually specify more than one error value,
as it is necessary to keep error messages and track from where an error
has been originated[^7].

This observation induces considering type inference as a **learning
problem** and allows finding the common ground between the dynamic and
static typing disciplines.

The languages relying on the static type discipline usually consider
'beyond' as a set of error messages, as a value should correspond to a
statically assigned and **narrow** type and mempty as a fully
polymorphic type for all values of a.

The languages corresponding to the dynamic type discipline consider
'beyond' as an untyped dynamic value and mempty as an unknown
polymorphic value[^8].

It should be noted that the Monoid operation is a union type
unification.

In addition, the standard laws for Monoid associated with to the
'beyond' set correspond to information addition by (\<\>a) or (a\<\>)
for any value of a.

As we denote it as **information acquisition**, we can define a better
interface as follows:

    class (Monoid   t
          ,Eq       t
          ,Show     t)
       =>  Typelike t where
       beyond :: t -> Bool

In this way, we can specify other elements in addition to a single
'beyond' one. When typing in Haskell encoding, we seek to enable the
"beyond" set to contain an error message.[^9]

Moreover, we find it useful to state which Typelike instances exactly
correspond to semilattices:

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

Concerning union types, we assume that there are many elements in the
"beyond" set. Its key property is that it corresponds to information
acquisition:

    typelikeSpec :: forall       ty.
                   (Typelike     ty
                   --,GenUnchecked ty
                   ,Typeable     ty
                   ,Arbitrary    ty)
                 => Spec
    typelikeSpec = describe ("Typelike " <> nameOf @ty) $ do
      {-commutativeSemigroupSpec @ty
      monoidSpec    @ty-}
      prop (nameOf  @ty <> " is commutative") $
        commutativeOnArbitrary @ty (<>)
      -- need beyond generator!
      {-prop (nameOf  @ty <> "beyond set is closed") $
        beyond_is_closed @ty-}

    beyond_is_closed :: forall   ty.
                        Typelike ty
                     => ty -> ty -> Property
    beyond_is_closed ty1 ty2 = do
      beyond (ty1 :: ty) ==> beyond (ty1 <> ty2)

It can be used as a convenient validation method when testing the
recursive structure of a type.

It should be noted that here, we abolish the semilattice requirement
that has been conventionally assumed for type constraints
\[\@semilattice\], as this requirement is valid only for strict type
constraint inference, not for a more general type inference considered
as a learning problem. As we observe in the example
\[\@example:row-constraint\], we need to perform non-monotonic inference
when dealing with alternative representations.

Typing relation

    class Typelike ty
       => ty `Types` val where
       infer :: val -> ty
       check :: ty -> val -> Bool

Here, the following notation is used:

-   \<\> denotes unification that is an associative commutative
    operation;

-   mempty is a neutral element of unification

-   the "beyond" set is an attractor of \<\> on both sides[^10].

In the case of union types for which we learn the shape of the type
based on examples, we can consider that:

-   mempty implies the case of *no information received*;

-   "beyond" represents the sets of Typelike objects corresponding to
    the cases of *no more information will change anything* or *no more
    information is accepted* (for the purpose of typing; the extra
    information may still be useful in error messages);

-   \<\> denotes fusing the information on the same entity from
    different observations.

We consider that a neutral element corresponds to *no information given*
or *no observation*, and that maximum is regarded to *no more
information accepted*.

Laws of typing

Specifying the laws of typing is important, as we may need to consider
separately the validity of domain of types/type constraints and that of
typing based on the terms corresponding to these valid types.

It should be noted that this approach is slightly more relaxed compared
with complete lattice subtyping \[\@subtype-inequalities\]
\[\@subtyping-lattice\], as it only considers semilattice corresponding
to unification operation.

Laws of typing {#laws-of-typing-1}
--------------

First, we note that to describe the case of *no information*, mempty
cannot correctly type any term:

    mempty_contains_no_terms
      :: forall    ty term.
         (Typelike ty
         ,Types    ty term)
      =>              term
      -> Expectation
    mempty_contains_no_terms term =
          check (mempty :: ty) term
            `shouldBe` False

It is also important for typing, as all terms are typed successfully by
any value in the "beyond" set.

    beyond_contains_all_terms :: forall ty  term.
                                (Types  ty    term
                                ,Show         term)
                              =>        ty -> term
                              -> Property
    beyond_contains_all_terms ty term = do
      beyond ty 
        term `shouldSatisfy` check ty

However, specifying types in the "beyond" set may be non-obvious for
particular instances. In this case, we can use special generator called
arbitrary. Beyond that is formulated as follows:


    {-
    beyond_contains_all_terms2 :: forall  ty term.
                                 (Typelike ty
                                 ,Types    ty term)
                               => term -> _
    beyond_contains_all_terms2 term =
      forAll arbitraryBeyond $ (`check` term)
      -}

We state an additional rule for typing: a type inferred from a term must
always be valid for that particular term.

    inferred_type_contains_its_term ::
         forall ty         term.
                ty `Types` term
      =>                   term
      -> Bool
    inferred_type_contains_its_term term =
      check ((infer:: term -> ty) term) (term :: term)

The law asserts that the scheme described in the following diagram is
presented.

![](media/image1.png){width="4.208333333333333in"
height="2.4305555555555554in"}

The last law states that the terms are correctly checked in terms of a
type after fusing more information within a single type:

    fusion_keeps_terms :: forall   ty v.
                         (Typelike ty
                         ,ty `Types` v)
                       => v -> ty -> ty -> Property
    fusion_keeps_terms v ty1 ty2 = do
      check ty1 v || check ty2 v ==>
        check (ty1 <> ty2) v

The minimal Typelike instance is the one that contains only "mempty"
corresponding to the case of *no sample data received* and "beyond" for
*all values permitted*.

It should be noted that these laws are compatible with the strict static
type discipline; namely, the "beyond" set corresponds to a set of type
errors, and a task for a compiler is to block any program with the terms
that type only to "beyond" as a least upper bound.

Type engineering principles
---------------------------

Considering that we aim to infer types from the finite number of
samples, we represented as a *learning problem* so that we need to use
*prior* knowledge about the domain to generalize while inferring types.

Observing that A; false, we can expect that in particular cases, we may
obtain that A: true. After noting that B=123, we expect that B=100 would
also be acceptable. It means that we need the considered typing system
to *learn a reasonable general class from few instances.* This motivates
formulating a practical type system as an inference problem.

As the purpose is to deliver the most descriptive[^11] types, we assume
that we need to obtain a wider view rather than focusing on a *free
type* and applying it to larger sets whenever it is deemed justified.

The other principle corresponds to **correct operation.** It implies
that having operations regarded to types, we can find a minimal set of
types that assure correct operation in the case of unexpected errors.

Indeed, we aim to apply this theory to infer a type definition from a
finite set of examples; however, we also seek to generalize it to
infinite types.

For this purpose, we set the following rules of type design:

-   type should have a finite description;

-   inference must be represented as a contravariant functor with
    regards to constructors. We note that for {\"a\": X, \"b\": Y},
    types correspond to T x y, and then X :: x and Y :: y must
    correspond to valid typing.

Simple type constraints can be summarized as follows:

1.  Given a sample of values, we can obtain a reasonable approximation
    of expected values:

-   We need to use String versus Int outright, unlike for any JSON
    value.

-   Assuming that we have a set of parsers that are mutually exclusive,
    we can implement this for String values: as follows.

Constraints on String type

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

Then, whenever unifying the String constraint, the following code can be
executed:

    instance Semigroup StringConstraint where
      SCNever    <>  a             = a
      a          <>  SCNever       = a
      SCAny      <>  _             = SCAny
      _          <>  SCAny         = SCAny
      SCDate     <>  SCDate        = SCDate
      SCEmail    <>  SCEmail       = SCEmail
      (SCEnum a) <> (SCEnum b)     |
              length (a <> b) < 10 = SCEnum (a <> b)
      _          <>  _             = SCAny

    instance Monoid StringConstraint where
      mappend = (<>)
      mempty  = SCNever

    instance Typelike StringConstraint where
      beyond  = (==SCAny)

Constraints on number type

Analogically, we may infer relying on integer constraints[^12] as
follows:

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

JavaScript provides one number type that contains both Float and Integer
so that the JSON values may inherit the following:

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
    NCFloat <> _       = NCFloat
    _       <> NCFloat = NCFloat
    NCNever <> _       = NCNever
    _       <> NCNever = NCNever

Free union type
---------------

Concerning a term with constructors, we can infer a "free" type for
every term as follows: for any T value, type set T\` satisfies the
notion of *free type* specified as follows:

    data FreeType a = FreeType { captured :: Set a }
                    | Full
      deriving (Eq, Ord, Show, Generic)

    instance (Ord a, Eq a) => Semigroup (FreeType a) where
      Full <> _    = Full
      _    <> Full = Full
      a    <> b    = FreeType
                   $ (Set.union `on` captured) a b
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

This definition is deemed sound and may be applicable to a finite set of
values. For a set of inputs: \[\"yes\", \"no\", \"error\"\], we may
reasonably consider that type is an appropriate approximation of C-style
enumeration or Haskell-style ADT without a need for using constructor
arguments.

However, the deficiency of this notion of *free type* is that it does
not allow generalizing to infinite and recursive domains. It only allows
utilizing the objects from the sample.

Presence and absence constraint
-------------------------------

We denote the case represented below as a *presence or absence
constraint*:

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

Although it does not deem useful in the context implying that we always
have at least one input value, it is important as it can be used to
specify an empty array (and therefore, no values for the element type.)

Then, we need to identify which of these types we may infer.

Presence and absence constraints

Given that after observing a true value, we also need expect false, we
can notice that the basic constraint for a Boolean value is grounded on
its presence or absence.

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

In this way, we obtain a basic presence constraint specified for any
non-void type.

It should be noted that the Boolean and null values are both addressed
by this trivial PresenceConstraint constraint.

In other words, we are only concerned about the presence or absence of
their corresponding observations. That means that the constraint for
Boolean is the simplest possible.

The same is valid for null values, as there is only one null value.

    type NullConstraint = PresenceConstraint ()

Selecting basic priors

Having type system engineering defined as prior selection, we clarify
obvious rules associated with typing.

We generalize the basic data types.

It should be noted that we consider nulls as a separate basic type that
can form a union with any other one. Therefore, in this case, "mempty"
indicates *no type and no value*. The "beyond" set corresponding to the
semilattice is the type of any value term.

Variants

Variant fields corresponding to union types are also simple so that they
can be implemented using a related class of either, assuming that these
that types are exclusive:

    data a :|: b = AltLeft  a
                 | AltRight b
      deriving (Show, Eq, Generic)

    instance (FromJSON  a
             ,FromJSON        b)
          =>  FromJSON (a :|: b) where
      parseJSON a =  AltLeft  <$> decodeEither
                 <|> AltRight <$> decodeEither

In other words for Int :\|: String type, we first control whether the
value is String, and if this check it fails, we attempt to parse it as
String.

Variant records are slightly more complicated as it may be unclear which
typing is better to use:

    {"message": "Where can I submit my proposal?",
        "uid" : 1014}
    {"error"  : "Authorization failed",
       "code" : 401}
    data OurRecord =
      OurRecord { message :: Maybe String
                , error   :: Maybe String
                , code    :: Maybe Int
                , uid     :: Maybe Int }

Or:

    data OurRecord2 = Message { message :: String
                              , uid     :: Int }
                    | Error   { error   :: String
                              , code    :: Int }

The best attempt here is to rely on the available examples being
reasonably exhaustive. That is, we can estimate how many examples we
have for each, and how many of them match. Then, we compare this number
with type complexity (with options being more complex). In such cases,
latter definition has only one choice (optionality), but we only have
two samples to begin with.

In the case of having more samples, the pattern emerges:

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

Object constraint

To avoid information loss, a constraint for JSON object type is
introduced in such a way to **simultaneously gather information** about
representing it either as a map or a record as follows:


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

Separately, we acquire the information about possible typing of a JSON
object as a record of values, as specified below:

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
        -- FIXME: treat extra keys!!!
        check rc obj
                | Map.keys (fields rc)
               == Map.keys        obj  =
          and $ Map.elems $
            Map.intersectionWith  check
                                 (fields rc)
                                  obj
        check _  _ = False

Observing that the two abstract domains considered above are
independent, we can store the information about both options separately
in a record as follows: [^13]

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

It should be noted that this representation is similar to *intersection
type*: any value that satisfies ObjectConstraint must conform by
containing mappingCase and recordCase.

It should be noted that this *intersection approach* to address
conflicting union type constraints benefits from *principal type
property*, meaning that a principal type is used to simply acquire the
information corresponding to different angles and handle it separately.

Array constraint

Similarly to the object type, ArrayConstraint is used to simultaneously
obtain the information about all possible representations of an array,
including the following:

-   an array of the same elements;

-   a row with the type depending on a column.

We need to acquire the information for both alternatives separately, and
then, to measure a relative likelihood of either cases before mapping
the union type to Haskell declaration.

Here, we specify the records for two different possible representations:


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

Row constraint

A row constraint is valid only if there is a fixed number of entries in
each row, which is represented by utilizing the "beyond" set whenever
there is an uneven number of columns.

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
      RowTop    <> _             = RowTop
      _         <> RowTop        = RowTop
      RowBottom <> a             = a
      a         <> RowBottom     = a
      Row bs    <> Row cs
        | length bs /= length cs = RowTop
      Row bs    <> Row cs        =
        Row $ zipWith (<>) bs cs

In other words, RowConstraint corresponds to a *levitated semilattice*
\[\@levitated-lattice\] with a neutral element over content type
\[UnionType\].

Combining the types into a union

As we have the union of only few possible constraints, we enable its
easier processing as follows.

It should be noted that given the constraints for different type
constructors, the union type can be considered as mostly a generic
Monoid object \[\@generic-monoid\] as follows:

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

The generic structure of union type can be explained by the fact that
the information contained in different record fields is *independent
from each other*. It means that we compute the meet over different
dimensions as follows:

    instance Monoid UnionType where
      mempty = UnionType {
          unionNull = mempty
        , unionBool = mempty
        , unionNum  = mempty
        , unionStr  = mempty
        , unionObj  = mempty
        , unionArr  = mempty
        }

As we described previously, the "beyond" set may correspond to either
**accepting any value** or to **accepting no more information**. Its
definition can be specified as follows:

    instance Typelike UnionType where
      beyond UnionType {..} =
          beyond unionNull
       && beyond unionBool
       && beyond unionNum
       && beyond unionStr
       && beyond unionObj
       && beyond unionArr

Inference breaks are used to disjoint alternatives according to
different record fields, depending on the constructor of a given value.

It enables implementing a clear and efficient approach that implies
distinguishing the values according to various dynamic types into
different partitions of the union[^14] as follows:

    instance UnionType `Types` Value where
      infer (Bool   b) = mempty { unionBool = infer b  }
      infer  Null      = mempty { unionNull = infer () }
      infer (Number n) = mempty { unionNum  = infer n  }
      infer (String s) = mempty { unionStr  = infer s  }
      infer (Object o) = mempty { unionObj  = infer o  }
      infer (Array  a) = mempty { unionArr  = infer a  }
      check UnionType { unionBool } (Bool   b) =
                  check unionBool          b
      check UnionType { unionNull }  Null      =
                  check unionNull         ()
      check UnionType { unionNum  } (Number n) =
                  check unionNum            n
      check UnionType { unionStr  } (String s) =
                  check unionStr            s
      check UnionType { unionObj  } (Object o) =
                  check unionObj            o
      check UnionType { unionArr  } (Array  a) =
                  check unionArr            a

Overlapping alternatives

The intersection of union type systems have been used to deal with the
conflicting types provided in the input.

Motivated by the examples considered above, we also aim to address
conflicting alternative assignments.

It is apparent that Examples 4-6 allow considering more than one
assignment:

1.  A set of lists of values that may correspond to Int, String, or
    null, or a table that has the same (and predefined) type for each
    value.

2.  A record of fixed names or the mapping from hash to a single object
    type.

Counting observations

In this section, we discuss how to ensure that we set an appropriate
number of samples. To explain this, the other example can be considered
as follows:

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

First, we need to identify it as a list of similar elements and then to
note that there are multiple instances of each record example. We
consider that the best approach would be to use the multisets of
inferred records instead of normal sets and then, to attempt minimizing
the term.

Next step is to detect the similarities between type descriptions
introduced for different parts of the term:

    {"samples"      :  [...],
     "last_message" : {"message": "Thanks!",
                          "uid" : 1014}
    }

We can add the auxiliary information about a number of samples observed,
and the constraint remains a Typelike object:

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

We can interconnect Counted as a parametric functor with types to track
auxiliary information.

It should be noted that the Counted constraint is the first example that
does not correspond to a semilattice, that is a\<\>a/=a.

This is because it is a Typelike object; however, it is not a type
constraint in a conventional sense. Instead, it is used to count the
number of samples observed for the constraint inside so that we can
decide on which alternative representation is supported by evidence in
the best manner.

Therefore, at each step, we may need to maintain a **cardinality** of
each possible value, and being provided with the sufficient number of
samples, we may attempt to detect patterns [^15].

To preserve efficiency, we may need to merge whenever the number of
alternatives in a multiset crosses the threshold.[^16] We can attempt to
narrow strings only in the cases when cardinality crosses the
threshold.[^17]

Selecting representations
=========================

Specifying heuristics to achieve better types
---------------------------------------------

The final step would be to perform the post-processing of an assigned
type before generating it to make it more resilient to common
uncertainties.

It should be noted that these assumptions may bypass the defined
validity criterion specified in the initial part of the paper; however,
they prove to work well in practice.

Array type with no element observations

If we have no observations corresponding to an array type, it can be
inconvenient to disable an array to contain any values at all.
Therefore, we introduce a non-monotonic step of converting "mempty" into
a final Typelike object aiming to introduce a representation allowing
the usage of any value in the input.

We note that the proposed program must not have any assumptions about
these values; however, at the same it should be able to output them for
debugging purposes.

Overall processing scheme
-------------------------

![](media/image2.png){width="5.277777777777778in"
height="2.111111111111111in"}

Simplification by identifying unification candidates
----------------------------------------------------

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

Scaling to type environments
----------------------------

In the present paper, we only discuss typing of tree-like values.
However, it is natural to scale this approach to multiple types in APIs
in which different types are referred to by name and possibly contain
each other.

To address these cases, we show that the environment of Typelike objects
also corresponds to a Typelike instance, and that constraint unification
can be extended in the same way.

Generic derivation of Typelike
------------------------------

It should be noted that Typelike instances for non-simple types usually
follow one of the following two patterns: 1) for typing terms that have
a finite sum of disjoint constructors, we bin this information using a
constructor during inference 2. for typing terms with two alternative
representations, we infer all constraints separately by applying
inference to the same term

In both cases, the derivation procedure of the Monoid, and Typelike
instances is the same.

It allows using GHC Generics \[\@generics, \[\@generic-monoid\]\] to
specify standard implementations for most of the boilerplate code.

It means that we only have to manually define the following: \* new
constraint types, \* inference from constructors (case 1), as well as
providing the entirety of handling alternative constraints until we
select representations.

Conclusion
----------

In the present study, we aimed to derive the types that were valid with
respect to the provided specification, thereby obtaining the information
from the input in the most comprehensive way.

We defined type inference as representation learning and type system
engineering as a meta-learning problem in which the **priors
corresponding to the data structure induced typing rules**.

We also formulated a mathematical formulation for the **union type
discipline** as a manipulation of bounded join-semilattices with a
neutral element that represented knowledge about the data structure.

In addition, we proposed a union type system engineering methodology
that was justified by a theoretical criteria. We demonstrated that it
was capable of consistently explaining the decisions made in practice.

We consider that this kind of *formally justified type system
engineering* can become widely used in practice, replacing *ad-hoc*
approaches in the future.

The proposed approach may be used to underlie the way towards formal
construction and derivation of type systems based on the specification
of value domains and design constraints.

Bibliography
============

Appendix: module headers
========================

Appendix: package dependencies
==============================

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
        source-dirs: test
        dependencies:
          - union-types
          - mtl
          - random
          - transformers
          - hashable
          - quickcheck-classes

Appendix: Hindley-Milner as Typelike
====================================

Appendix: Missing pieces of code
================================


    isValidDate :: Text -> Bool
    isValidDate = isJust
                . parseISO8601
                . Text.unpack

    isValidEmail :: Text -> Bool
    isValidEmail = Text.Email.Validate.isValid
               . Text.encodeUtf8

Appendix: Less arbitrary wait time
==================================

    {-# language DefaultSignatures     #-}
    {-# language FlexibleInstances     #-}
    {-# language FlexibleContexts      #-}
    {-# language GeneralizedNewtypeDeriving #-}
    {-# language Rank2Types            #-}
    {-# language MultiParamTypeClasses #-}
    {-# language ScopedTypeVariables   #-}
    {-# language StandaloneDeriving    #-}
    {-# language TypeOperators         #-}
    {-# language TypeFamilies          #-}
    {-# language TypeApplications      #-}
    {-# language TupleSections         #-}
    {-# language UndecidableInstances  #-}
    {-# language AllowAmbiguousTypes   #-}
    {-# language DataKinds             #-}
    {-# language KindSignatures        #-}
    module LessArbitrary(
        LessArbitrary(..)
      , oneof
      , CostGen(..)
      , (<$$$>)
      , (<$$$?>)
      , currentBudget
      , fasterArbitrary
      , genericLessArbitrary
      , genericLessArbitraryMonoid
      , flatLessArbitrary
      , spend
      ) where

    import qualified Data.HashMap.Strict as Map
    import qualified Data.Set            as Set
    import qualified Data.Vector         as Vector
    import qualified Data.Text           as Text
    import qualified Data.Text.Encoding  as Text
    import Control.Monad(replicateM)
    import Data.Scientific
    import Data.Aeson
    import Data.Proxy
    import Data.Typeable
    import Test.Hspec
    import Test.Hspec.QuickCheck
    import qualified Test.QuickCheck as QC
    import qualified Test.QuickCheck.Gen as QC
    import Test.Hspec
    import Test.Hspec.QuickCheck
    --import Test.QuickCheck.Arbitrary.Generic
    import Test.Validity hiding(check)
    import Test.Validity.Monoid
    import Test.Validity.Shrinking
    import Test.Validity.Utils(nameOf)
    import qualified Control.Monad.State.Strict as State
    import Control.Monad.Trans.Class
    import System.Random(Random)
    import Control.Applicative
    import Data.Proxy
    import GHC.Generics as G
    import GHC.TypeLits
    import qualified Test.QuickCheck as QC
    import qualified Test.QuickCheck.Arbitrary as QC
    import Data.Hashable

    newtype Cost = Cost { unCost :: Int }
      deriving (Eq,Ord,Enum,Bounded,Num)

    newtype CostGen                               a =
            CostGen {
              runCostGen :: State.StateT Cost QC.Gen a }
      deriving (Functor, Applicative, Monad, State.MonadFix)

    -- Mark a costly constructor with this instead of `<$>`
    (<$$$>) :: (a -> b) -> CostGen a -> CostGen b
    costlyConstructor <$$$> arg = do
      spend 1
      costlyConstructor <$> arg

    spend :: Cost -> CostGen ()
    spend c = CostGen $ State.modify (-c+)

    oneof   :: [CostGen a] -> CostGen a
    oneof [] = error "LessArbitrary.oneof used with empty list"
    oneof gs = choose (0,length gs - 1) >>= (gs !!)

    elements :: [a] -> CostGen a
    elements gs = (gs!!) <$> choose (0,length gs - 1)

    choose      :: Random  a
                =>        (a, a)
                -> CostGen a
    choose (a,b) = CostGen $ lift $ QC.choose (a, b)

    (<$$$?>) :: CostGen a -> CostGen a -> CostGen a
    cheapVariants <$$$?> costlyVariants = do
      budget <- CostGen State.get
      if budget > (0 :: Cost)
         then costlyVariants
         else cheapVariants

    currentBudget :: CostGen Cost
    currentBudget = CostGen State.get

    -- | Chooses one of the given generators, with a weighted random distribution.
    -- The input list must be non-empty.
    frequency :: [(Int, CostGen a)] -> CostGen a
    frequency [] = error "LessArbitrary.frequency used with empty list"
    frequency xs
      | any (< 0) (map fst xs) =
        error "LessArbitrary.frequency: negative weight"
      | all (== 0) (map fst xs) =
        error "LessArbitrary.frequency: all weights were zero"
    frequency xs0 = choose (1, tot) >>= (`pick` xs0)
     where
      tot = sum (map fst xs0)

      pick n ((k,x):xs)
        | n <= k    = x
        | otherwise = pick (n-k) xs
      pick _ _  = error "LessArbitrary.pick used with empty list"

    costFrequency :: [(Int, CostGen a)] -> CostGen a
    costFrequency [] = error "LessArbitrary.costFrequency used with empty list"
    costFrequency xs = do
      budget <- currentBudget
      if budget>0
        then frequency                  xs
        else frequency
           $ filter ((minFreq==) . fst) xs
      where
        minFreq = minimum $ map fst xs


    withCost :: Int -> CostGen a -> QC.Gen a
    withCost cost gen = runCostGen gen
      `State.evalStateT` Cost cost

    defaultCost :: Int
    defaultCost  = 100

    {-
    instance LessArbitrary a
          => QC.Arbitrary  a where
      arbitrary = withCost defaultCost $ lessArbitrary
      -}

    class LessArbitrary a where
      lessArbitrary :: CostGen a
      default lessArbitrary :: (Generic a, CGArbitrary (Rep a)) => CostGen a
      lessArbitrary = to <$> cgArbitrary
      
    class CGArbitrary a where
      cgArbitrary :: CostGen (a x)

    instance CGArbitrary G.U1 where
      cgArbitrary = pure G.U1

    instance LessArbitrary       c
          => CGArbitrary (G.K1 i c) where
      cgArbitrary = G.K1 <$> lessArbitrary

    instance CGArbitrary f
          => CGArbitrary (G.M1 i c f) where
      cgArbitrary = G.M1 <$> cgArbitrary

    instance (CGArbitrary a, CGArbitrary b) => CGArbitrary (a G.:*: b) where
      cgArbitrary = liftA2 (G.:*:) cgArbitrary cgArbitrary

    -- | Calculates count of constructors encoded by particular ':+:'.
    -- Internal use only.
    type family SumLen a :: Nat where
      SumLen (a G.:+: b) = (SumLen a) + (SumLen b)
      --SumLen (a G.:*: b) = (SumLen a) + (SumLen b)
      SumLen  a          =  1

    type family ConsCost a :: Nat where
      ConsCost (a G.:*: b) =     (ConsCost a) + (ConsCost b)
      ConsCost (a G.:+: b) = Min (ConsCost a) (ConsCost b)
      ConsCost  a          = 1

    instance (CGArbitrary      a,  CGArbitrary      b,
              KnownNat (SumLen a), KnownNat (SumLen b)
             ) => CGArbitrary (a G.:+:              b) where
      cgArbitrary = costFrequency
        [ (lfreq, G.L1 <$$$> cgArbitrary)
        , (rfreq, G.R1 <$$$> cgArbitrary) ]
        where
          lfreq = fromIntegral $ natVal (Proxy :: Proxy (SumLen a))
          rfreq = fromIntegral $ natVal (Proxy :: Proxy (SumLen b))

    fasterArbitrary :: LessArbitrary a => QC.Gen a
    fasterArbitrary = sizedCost lessArbitrary

    genericLessArbitrary :: (Generic a, CGArbitrary (Rep a)) => CostGen a
    genericLessArbitrary = to <$> cgArbitrary

    genericLessArbitraryMonoid :: (Generic          a
                                  ,CGArbitrary (Rep a)
                                  ,Monoid           a)
                               =>  CostGen          a
    genericLessArbitraryMonoid  =
      pure mempty <$$$?> genericLessArbitrary

    type family Min m n where
      Min m n = Min_ m n (CmpNat m n)

    type family Min_ (m::Nat) (n::Nat) (o::Ordering) where 
      Min_ m n 'LT = m
      Min_ m n 'EQ = m
      Min_ m n 'GT = n

    instance LessArbitrary Bool where
      lessArbitrary = flatLessArbitrary

    instance LessArbitrary Int where
      lessArbitrary = flatLessArbitrary

    instance LessArbitrary Integer where
      lessArbitrary = flatLessArbitrary

    instance LessArbitrary Double where
      lessArbitrary = flatLessArbitrary

    instance LessArbitrary Char where
      lessArbitrary = flatLessArbitrary

    instance (LessArbitrary k
             ,LessArbitrary   v)
          => LessArbitrary (k,v) where

    instance (LessArbitrary          k
             ,Ord                    k)
          =>  LessArbitrary (Set.Set k) where
      lessArbitrary = Set.fromList <$> lessArbitrary

    instance (LessArbitrary              k
             ,Eq                         k
             ,Ord                        k
             ,Hashable                   k 
             ,LessArbitrary                v)
          =>  LessArbitrary (Map.HashMap k v) where
      lessArbitrary =  Map.fromList
                   <$> lessArbitrary

    instance LessArbitrary  a
          => LessArbitrary [a] where
      lessArbitrary = pure [] <$$$?> do
        len  <- choose (1,100) -- FIXME: use sized
        spend $ Cost len
        replicateM   len lessArbitrary

    instance LessArbitrary Scientific where
      lessArbitrary =
        scientific <$> lessArbitrary
                   <*> lessArbitrary

    flatLessArbitrary :: QC.Arbitrary a
                  => CostGen a
    flatLessArbitrary  = CostGen $ lift QC.arbitrary

    instance LessArbitrary                a
          => LessArbitrary (Vector.Vector a) where
      lessArbitrary = Vector.fromList <$> lessArbitrary

    sizedCost :: CostGen a -> QC.Gen a
    sizedCost gen = QC.sized $ (`withCost` gen)

    instance QC.Testable          a
          => QC.Testable (CostGen a) where
      property = QC.property
               . sizedCost

    forAll gen prop = do
      gen >>= prop

[^1]: Or at least beyond bottom expanding to *infamous undefined
    behavior* \[\@undefined1, \[\@undefined2\], \[\@undefined3\]\].

[^2]: Which is considered as a bad practice; however, it is part of
    real-life APIs. We may need to make it optional using the
    \--array-records option.

[^3]: Example is taken from \[\@quicktype\].

[^4]: Compiler feature of checking for unmatched cases.

[^5]: As used by the Aeson \[\@aeson\] package.

[^6]: JavaScript and JSON use a binary floating point instead; however,
    we follow the representation selected by Aeson library that parses
    JSON.

[^7]: In this case: beyond (Error \_) = True \| otherwise = False.

[^8]: May sound similar until we consider adding more information to the
    type.

[^9]: It should be noted that many but not all type constraints are
    semilattice. Please refer to the counting example below.

[^10]: For all a. (\<\> a) and ∀ a.(a\<\>), the result is kept in the
    "beyond" set.

[^11]: The shortest one according to the information complexity
    principle.

[^12]: The program makes it optional \--infer-int-ranges.

[^13]: Choice of representation will be explained later. Here we only
    consider acquiring the information about possible values.

[^14]: The question may arise: what is the *union type* without *set
    union*? When the sets are disjoint, we put the values in different
    bins to enable easier handling.

[^15]: If we detect a pattern too early, we risk to make the types too
    narrow to work with actual API answers.

[^16]: Option \--max-alternative-constructors=N.

[^17]: Option \--min-enumeration-cardinality.
