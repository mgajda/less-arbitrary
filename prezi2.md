---
title:  "Towards a more perfect union type"
description: |
  Proposes a new framework for union types
  that uses monoid and complete functions,
  and more accessible theoretical treatment
  than ad-hoc typing relations.
link: "https://gitlab.com/migamake/json-autotype"
author:
  - Michał J. Gajda
  - "https://www.migamake.com"
author-meta: "Migamake Pte Ltd"
tags:
  - Haskell
  - JSON
  - union type
  - type providers
abstract: |
  We present a principled theoretical
  framework for inferring and checking the union types,
  and show its work in practice
  on JSON data structures.

  The framework poses a union type inference as a learning problem
  from multiple examples.
  The categorical framework is generic and easily extensible.
date:   2020-09-04
tables: true
listings: true
header-includes: |
  \usepackage{tikz}
  \usepackage{tikz-cd}
---

# Plan

::: incremental

* Typing dynamic languages
* JSON
* Typelike framework
* Examples of implementation
* Summary

:::

# JSON

```{.json}
{"version": 1.0
,"author":  "Pole Anonymous"
,"sections": [
    "Introduction"
  , "Materials"
  , "Methods"
  , "Closure"
  ]}
```

. . .

``` {.haskell file=refs/Data/Aeson.hs}
data Value = Object (Map String Value)
           | Array [Value]
           | Null
           | Number Scientific
           | String Text
           | Bool Bool
```

# Related work

* quicktype
* F# type providers
* XDuce and Castagna framework

# Key features

* Decidability
* Soundness
* Subject reduction

# Motivation

Subsets of data within a single constructor:

::: incremental


*   *API argument is an email* -- it is a subset of valid `String`
  values that can be validated on the client-side.
*   *The page size determines the number of results to return (min: 10,
  max:10,000)* -- it is also a subset of integer values (`Int`) between $10$, and $10,000$
* _The `date` field contains ISO8601 date_ -- a record field represented
  as a `String` that contains a calendar date in the format
  `"2019-03-03"`

:::

. . .

``` {.haskell language="Haskell" file=test/example1a.result .hidden}
newtype Example1a = Example Email
```

# Motivation: optional fields

*The page size is equal to 100 by default*

```{.json}
{}
{"page_size": 50}
```

. . .

```{.haskell file=test/example2.result .hidden}
newtype Example2 = Example2 { page_size :: Maybe Int }
```

# Motivation: variant records

*Answer contains either a text message with a user identifier or an error.*

``` {.json file=test/example4.json}
{"message" : "Where can I submit my proposal?",
 "uid"     : 1014}
{"message" : "Submit it to HotCRP",
 "uid"     :  317}
{"error"   : "Authorization failed",
 "code"    :  401}
{"error"   : "User not found",
 "code"    :  404}
```

```{.haskell file=test/example4.result}
data Example4 = Message { message :: String, uid  :: Int }
              | Error   { error   :: String, code :: Int }
```

# Motivation: alternative objects

*Answer to a query is either a number of registered objects, or an identifier of a singleton object.*

``` {.haskell file=test/example3.result .hidden}
newtype Example3 = Example3 (String :|: Int)
```



# Motivation: array of records

``` {.json file=test/example5.json #lst:row-constraint}
[ [1, "Nick",    null       ]
, [2, "George", "2019-04-11"]
, [3, "Olivia", "1984-05-03"] ]
```

``` {.haskell file=test/example5.result}
data Examples = Examples [Example]

data Example5 = Example5 { col1 :: Int
                         , col2 :: String
                         , col3 :: Maybe Date }
```

# Motivation: maps

Example of map of identical objects:

``` {.json file=test/example6.json}
{   "6408f5": { "size":       969709
              , "height":    510599
              , "difficulty": 866429.732
              , "previous": "54fced" },
    "54fced": { "size":       991394
              , "height":    510598
              , "difficulty": 866429.823
              , "previous": "6c9589" },
    "6c9589": { "size":       990527
              , "height":     510597
              , "difficulty": 866429.931
              , "previous":  "51a0cb"
              }
}
```

# Motivation: **objects** vs maps

``` {.haskell file=test/example6-single-key.result}
data Example = Example { f_6408f5 :: O_6408f5
                       , f_54fced :: O_6408f5
                       , f_6c9589 :: O_6408f5 }
data O_6408f5 = O_6408f5 {
         size       :: Int
       , height     :: Int
       , difficulty :: Double
       , previous   :: String }
```

# Motivation: objects vs **maps**

```{.haskell file=test/example6-multi-key.result}
data ExampleMap = ExampleMap (Map Hex ExampleElt)
data ExampleElt = ExampleElt {
        size       :: Int
      , height     :: Int
      , difficulty :: Double
      , previous   :: String }
```

::: notes

It should be noted that the last example presented above requires
Haskell representation inference to be non-monotonic,
as an example of object with only a single key would be best represented by a record type:

:::

# Goals

* detect unexpected format deviations
* detect need for program updates
* minimal containing set
* information content
* correct operation
* inference as contravariant functor

::: notes

For example, if `AType x y` types `{"a": X, "b": Y}`, then
`x` must type `X`, and `y` must type `Y`.

:::

# Type inference

Information fusion

::: incremental

* unification
* or anti-unification

``` {.haskell file=refs/Data/Semigroup.hs}
class Semigroup ty              where (<>)   :: ty -> ty -> ty
class Semigroup ty => Monoid ty where mempty :: ty
```

:::


# Beyond set

```{.haskell #typelike }
class (Monoid t, Eq t, Show t) => Typelike t where beyond :: t -> Bool
```

The `beyond` set is always **closed to information
addition** by `(<>a)` or `(a<>)` for any value of `a`, or **submonoid**.

Do not require _idempotence_, nor _commutativity_ of `<>`.

# Typelike

``` {.haskell #typelike }
class Typelike ty => ty `Types` val where
   infer ::       val -> ty
   check :: ty -> val -> Bool
```

# Laws of Typelike

 $$ \begin{array}{l l l l lllllcr}
                    &     &   &             & \textrm{check} & \textrm{mempty} & v & = & \textbf{False} & \ &  \\
    \textrm{beyond} & t   &   & \Rightarrow & \textrm{check} & t               & v & = & \textbf{True} & \hspace{1cm}\hfill{} &  \\
    \textrm{check}  & t_1 & v & \Rightarrow & \textrm{check} & (t_1 \diamond t_2) & v & = & \textbf{True} & \hspace{1cm}\hfill{} &  \\
    \textrm{check}  & t_2 & v & \Rightarrow & \textrm{check} & (t_1 \diamond t_2) & v & = & \textbf{True} & \hspace{1cm}\hfill{} &  \\
                    &     &   &             & \textrm{check} & (\textrm{infer}\ v) & v & = & \textbf{False} & \ &  \\
                    &     &   &             \multicolumn{4}{r}{t_1 \diamond (t_2 \diamond t_3)} & = & t_1 \diamond (t_2 \diamond t_3) & \ &  \\
                    &     &   &             \multicolumn{4}{r}{\textrm{mempty} \diamond t} & = & t & \ &  \\
                    &     &   &             \multicolumn{4}{r}{t \diamond \textrm{mempty}} & = & t & \ &  \\
    \end{array} $$

# Typelike {.fragile}

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

# Presence and absence constraint

``` {#presence-absence-constraints .haskell}
data PresenceConstraint a = Present -- beyond
                          | Absent  -- mempty

instance Semigroup (PresenceConstraint a) where
  Absent  <> a       = a
  a       <> Absent  = a
  Present <> Present = Present

instance PresenceConstraint a `Types` a where
  infer _         = Present
  check Present _ = True
  check Absent  _ = False
```

# Flat type constraints

```{.haskell}
data NumberConstraint = NCInt
                      | NCNever -- mempty
                      | NCFloat -- beyond

instance Semigroup NumberConstraint where
  NCInt   <> NCInt   = NCInt
  NCFloat <> _       = NCFloat -- beyond
  _       <> NCFloat = NCFloat -- beyond
  NCNever <> a       = a       -- mempty
  a       <> NCNever = a       -- mempty

instance NumberConstraint `Types` Scientific where
  infer sci | base10Exponent sci >= 0 = NCInt
  infer _                             = NCFloat
  check NCInt   sci = base10Exponent sci >= 0
  check NCFloat _   = True
  check NCNever _   = False
```

# Cost of optionality


```{.haskell #typecost}
class Typelike ty => TypeCost ty where
  typeCost ::  ty -> TyCost
  typeCost a = if a == mempty then 0 else 1

instance Semigroup TyCost where (<>)   = (+)
instance Monoid    TyCost where mempty = 0

newtype TyCost = TyCost Int
```

# Mapping constraint

``` {.haskell }
data MappingConstraint =
    MappingNever -- mempty
  | MappingConstraint { keyConstraint  
                          :: StringConstraint
                      , valueConstraint
                          :: UnionType        }
```

# Mapping constraint 2

```{.haskell}
instance Semigroup   MappingConstraint where
  MappingNever <> a = a  
  a <> MappingNever = a  
  a <> b = MappingConstraint {
      keyConstraint   =
        ((<>) `on` keyConstraint  ) a b
    , valueConstraint =
        ((<>) `on` valueConstraint) a b
    }
```

# Record constraint

``` {.haskell #object-constraint}
data RecordConstraint =
    RCTop    {- beyond -}
  | RCBottom {- mempty -}
  | RecordConstraint { fields :: HashMap Text UnionType }

instance Semigroup   RecordConstraint where
  RecordConstraint     a  <>
    RecordConstraint     b = RecordConstraint $
         Map.intersectionWith (<>) a b
      <> (makeNullable <$> mapXor  a b)
```

# RecordConstraint 2

``` {#object-constraint .haskell}
instance RecordConstraint `Types` Object where
    infer = RecordConstraint    . Map.fromList
          . fmap (second infer) . Map.toList
    check RecordConstraint {fields} obj =
         all (`elem` Map.keys fields)
                    (Map.keys  obj)
      && and (Map.elems $ Map.intersectionWith
                            check fields obj)
      && all isNullable (Map.elems
                        $ fields `Map.difference` obj)
         -- absent values are nullable
```

# Object constraint

```{.haskell}
data ObjectConstraint = ObjectNever -- mempty
  |  ObjectConstraint { mappingCase :: MappingConstraint
                      , recordCase  :: RecordConstraint } 

instance Semigroup ObjectConstraint where
  a <> b = ObjectConstraint {
             mappingCase = ((<>) `on` mappingCase) a b
           , recordCase  = ((<>) `on` recordCase ) a b
           }
       
instance ObjectConstraint `Types` Object where
  infer v = ObjectConstraint (infer v) (infer v)
```

# Array constraint


``` {.haskell #array-constraint}
data ArrayConstraint =
     ArrayNever -- mempty
   | ArrayConstraint { rowCase   :: RowConstraint,
                     , arrayCase :: UnionType }

instance Semigroup ArrayConstraint where
  a1 <> a2 =
    ArrayConstraint {
      rowCase   = ((<>) `on` rowCase  ) a1 a2
    , arrayCase = ((<>) `on` arrayCase) a1 a2
    }

instance ArrayConstraint `Types` Array where
    infer vs = ArrayConstraint {
        rowCase = infer vs
      , arrayCase = mconcat (infer <$> Foldable.toList vs)
      }
```

# Row constraint

``` {#row-constraint .haskell}
data RowConstraint = RowTop | RowNever | Row [UnionType]

instance Semigroup RowConstraint where
  Row bs <> Row cs | length bs /= length cs = RowTop
  Row bs <> Row cs = Row $ zipWith (<>) bs cs

instance RowConstraint `Types` Array where
  infer = Row
        . Foldable.toList
        . fmap infer
  check (Row rs) vs
    | length rs == length vs =
      and $
        zipWith check                 rs
                     (Foldable.toList vs)
```

# Union type

``` {#type .haskell}
data UnionType = UnionType {
    unionNull :: NullConstraint
  , unionBool :: BoolConstraint
  , unionNum  :: NumberConstraint
  , unionStr  :: StringConstraint
  , unionArr  :: ArrayConstraint
  , unionObj  :: ObjectConstraint }
```

# Union type 2
``` {.haskell}
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

# Union type 3
```{.haskell #union-type-instance .hidden}
-- Since union type is all about optionality,
-- we need to sum all options from different alternatives:
instance TypeCost UnionType where
  typeCost UnionType {..} = typeCost unionBool
     + typeCost unionNull + typeCost unionNum
     + typeCost unionStr  + typeCost unionObj
     + typeCost unionArr
```

# Counting observations

```{#counted .haskell}
data Counted a = Counted { count :: Int, constraint :: a }

instance Semigroup a => Semigroup (Counted a) where
  a <> b = Counted (count      a +  count      b)
                   (constraint a <> constraint b)

instance          ty  `Types` term
      => (Counted ty) `Types` term where
  infer term = Counted 1 $ infer term
  check (Counted _ ty) term = check ty term
```

# Summary

::: incremental

* Monoid-based exposition of type inference
* Typelike as non-lossy learning
* Generic monoid suffices
* Easy extensibility
* Liberal laws
* Next version of `json-autotype`

:::
