---
title:  "Less Arbitrary waiting time"
shorttitle: "Less Arbitrary"
subtitle: "Short paper"
author:
  - name: Michał J. Gajda
    orcid:       "0000-0001-7820-3906"
affiliation:
  institution: "Migamake Pte Ltd"
  email:        mjgajda@migamake.com
  url:         "https://migamake.com"
tags:
  - Haskell
  - property testing
  - QuickCheck
abstract: |
  Property testing is the cheapest and most precise way of building up a test suite for your program.
  Especially if the datatypes enjoy nice mathematical laws.
  But it is also the easiest way to make it run for an unreasonably long time.
  We prove connection between deeply recursive data structures, and epidemic growth rate, and show how to fix the problem, and make Arbitrary instances run in linear time with respect to assumed test size.
date:   2020-05-05
description: |
link: "https://gitlab.com/migamake/less-arbitrary"
bibliography:
  - less-arbitrary.bib
conference: PREP
prologue: |
  \let\longtable\tabular
  \let\endlongtable\endtabular
  \renewcommand{\url}[1]{\href{#1}{[Link]}}
link-citations: true
tables: true
listings: true
acks: |
  The text was prepared with great help of bidirectional literate programming[@literate-programming] tool[@entangled],
  Pandoc[@pandoc] markdown publishing system and live feedback from Stack file watching option[@haskell-stack].

---

# Introduction

Property testing is the cheapest and most precise way of building up a test suite for your program.
Especially if the datatypes enjoy nice mathematical laws.
But it is also the easiest way to make it run for an unreasonably long time.
We show that connection between deeply recursive data structures, and epidemic growth rate
can be easily fixed with a generic implementation.
After our intervention the Arbitrary instances run in linear time with respect to assumed test size.
We also provide a fully generic implementation, so error-prone coding process is removed.

# Motivation

Typical arbitrary instance just draws
a random constructor from a set,
possibly biasing certain outcomes.

**Generic** arbitrary instance looks like this:

```{.haskell #tree-type}
data Tree        a =
    Leaf         a
  | Branch [Tree a]
  deriving (Eq,Show,Generic.Generic)
```
```{.haskell #tree-type-typical-arbitrary}
instance Arbitrary       a
      => Arbitrary (Tree a) where
  arbitrary = oneof [Leaf   <$> arbitrary
                    ,Branch <$> arbitrary
                    ]
```

Assuming we run QuickCheck with any size parameter greater than 1,
it will fail to terminate!

List instance is a wee bit better,
since it tries to limit maximum list length to a constant
option:

```{.haskell}
instance Arbitrary  a
      => Arbitrary [a] where
  lessArbitrary = sized $ \size do
    len  <- choose (1,size)
    vectorOf len lessArbitrary
```

Indeed QuickCheck manual[@quickcheck-manual],
suggests an error-prone, manual method of limiting
the depth of generated structure by dividing `size` by reproduction factor
of the structure^[We changed `liftM` and `liftM2` operators to `<$>` and `<*>` for clarity and consistency.] :

```{.haskell}
data Tree = Leaf Int | Branch Tree Tree

instance Arbitrary Tree where
  arbitrary = sized tree'
    where tree' 0 = Leaf <$> arbitrary
	  tree' n | n>0 = 
		oneof [Leaf   <$> arbitrary
   	      ,Branch <$> subtree <*> subtree]
  	    where subtree = tree' (n `div` 2)
```

Above example uses division of size by maximum branching factor
to decrease coverage into relatively deep data structures,
whereas dividing by average branching factor of `~2` will
generate both deep and very large structures.

This fixes non-termination issue, but still may lead to unpredictable waiting times for nested structures.
The depth of the generated structure is linearly limited
by dividing the `n` by expected branching factor of the recursive data structure.
However this does not work very well for mutually recursive data structures
occuring in compilers[@compilersALaCarte],
which may have 30 constructors with highly variable^[Due to list parameters.] branching factor
just like GHC's `HSExpr` data types.

Now we have a choice of manual generation of these data structures,
which certainly introduces bias in testing, or abandoning property testing
for real-life-sized projects.

Another motivation is that more complex data structures like
lambda terms require additional information for their generation.
For example a list of free variables.
That means that for generating complex structures we need to pass
a state through the test case generator.
It would be much more convenient to have an explicit state assigned to each generated
type, so we may use type classes to generate nested data structures.

# Complexity analysis

We might be tempted to compute average size of the structure.
Let's use reproduction rate estimate for a single
rewrite of `arbitrary` function written in conventional way.

We compute a number of recursive references for each constructor.
Then we take an average number of references among all the constructors.
If it is greater than 1, any **non-lazy** property test will certainly fail to terminate.
If it is slightly smaller, we still can wait a long time.

What is an issue here is not just non-termination which is fixed by error-prone manual process
of writing own instances that use explicit `size` parameter.

The much worse issue is unpredictability of the test runtime. Final issue is the poor coverage
for mutually recursive data structure with multitude of constructors.

Given a _maximum size_ parameter (as it is now called) to QuickCheck,
would we not expect that tests terminate within linear time of this parameter?
At least if our computation algorithms are linear with respect to input size?

Currently for any recursive structure like `Tree a`,
we see some exponential function. For example $size^n$, where $n$ is a random variable.

# Solution

We propose to replace implementation with a simple state monad[@composing-monads] that actually
remembers how many constructors were generated,
and thus avoid limiting the depth of generated data structures,
and ignoring estimation of branching factor altogether.

```{.haskell #cost}
newtype Cost = Cost Int
  deriving (Eq,Ord,Enum,Bounded,Num)
```

```{.haskell #costgen}
newtype CostGen s a =
        CostGen {
          runCostGen :: State.StateT (Cost, s) QC.Gen a
        }
  deriving (Functor, Applicative, Monad, State.MonadFix)

instance State.MonadState        s  (CostGen s) where
  state :: forall s a.
           (s -> (a, s)) -> CostGen s a
  state nestedMod = CostGen $ State.state mod
    where
      mod :: (Cost, s) -> (a, (Cost, s))
      mod (aCost, aState) = (result, (aCost, newState))
        where
          (result, newState) = nestedMod aState
```

We track the spending in the usual way:
```{.haskell #spend}
spend :: Cost -> CostGen s ()
spend c = do
  CostGen $ State.modify (first (-c+))
  checkBudget
```

To make generation easier, we introduce `budget check` operator:
```{.haskell #budget}
($$$?) :: HasCallStack
       => CostGen s a
       -> CostGen s a
       -> CostGen s a
cheapVariants $$$? costlyVariants = do
  budget <- fst <$> CostGen State.get
  if | budget > (0 :: Cost) -> costlyVariants
     | budget > -10000      -> cheapVariants
     | otherwise            -> error $
       "Recursive structure with no loop breaker."
```

```{.haskell #budget}
checkBudget :: HasCallStack => CostGen s ()
checkBudget = do
  budget <- fst <$> CostGen State.get
  if budget < -10000
    then error "Recursive structure with no loop breaker."
    else return ()
```

In order to conveniently define our budget generators,
we might want to define a class for them:

```{.haskell #less-arbitrary-class}
class StartingState s
   => LessArbitrary s a where
  lessArbitrary :: CostGen s a
```

Note that starting state can default to `()`:
```{.haskell #starting-state}
class StartingState s where
  startingState :: s

instance StartingState () where
  startingState = ()
```

```{.haskell #less-arbitrary-class .hidden}
  default lessArbitrary :: (Generic               a
                           ,GLessArbitrary s (Rep a))
                        =>  CostGen        s      a
  lessArbitrary = genericLessArbitrary
```

Then we can use them as implementation of `arbitrary`
that should have been always used:

```{.haskell #arbitrary-implementation}
fasterArbitrary :: forall        s a.
                   LessArbitrary s a
                => QC.Gen          a
fasterArbitrary  = (sizedCost :: CostGen s a -> QC.Gen a) (lessArbitrary :: CostGen s a)

sizedCost :: LessArbitrary s a
          => CostGen       s a
          -> QC.Gen          a
sizedCost gen = QC.sized (`withCost` gen)
```

Then we can implement `Arbitrary` instances simply with:

```{.haskell}
instance _
      => Arbitrary a where
  arbitrary = fasterArbitrary
```

Of course we still need to define `LessArbitrary`,
but after seeing how simple was a `Generic` defintion `Arbitrary`
we have a hope that our implementation will be:


```{.haskell}
instance LessArbitrary where
```
That is - we hope that the the generic implementation will take over.

# Introduction to GHC generics

Generics allow us to provide default instance,
by encoding any datatype into its generic `Rep`resentation:
```{.haskell #generic-instance}
instance Generics (Tree a) where
  to   :: Tree a -> Rep (Tree a)
  from :: Rep (Tree a) -> Tree a
```

The secret to making a generic function
is to create a set of `instance` declarations
for each type family constructor.

So let's examine `Rep`resentation of our working example,
and see how to declare instances:

1. First we see datatype metadata `D1` that shows where our type was defined:
```{.haskell #generic-instance-other}
type instance Rep (Tree a) =
  D1
   ('MetaData "Tree"
              "Test.Arbitrary"
              "less-arbitrary" 'False)
```

2. Then we have constructor metadata `C1`:
```{.haskell}
       (C1
          ('MetaCons "Leaf" 'PrefixI 'False)
```
3. Then we have metadata for each field selector within a constructor:
```{.haskell}
          (S1
             ('MetaSel
                'Nothing
                'NoSourceUnpackedness
                'NoSourceStrictness
                'DecidedLazy)
```
4. And reference to another datatype in the record field value:
```{.haskell}
             (Rec0 a))
```
5. Different constructors are joined by sum type operator:
```{.haskell}
        :+:
```
6. Second constructor has a similar representation:
```{.haskell}
        C1
              ('MetaCons "Branch" 'PrefixI 'False)
              (S1
                 ('MetaSel
                    'Nothing
                    'NoSourceUnpackedness
                    'NoSourceStrictness
                    'DecidedLazy)
                    (Rec0 [Tree a])))
                     ignored
```
7. Note that `Rep`resentation type constructors have additional parameter that is not relevant for our use case.

For simple datatypes, we are only interested in three constructors:

* `:+:` encode choice between constructors
* `:*:` encode a sequence of constructor parameters
* `M1`  encode metainformation about the named constructors, `C1`, `S1` and `D1` are actually
        shorthands for `M1 C`, `M1 S` and `M1 D`

There are more short cuts to consider:
* `U1`  is the unit type (no fields)
* `Rec0` is another type in the field

## Example of generics

This generic representation can then be matched by generic instances.
Example of `Arbitrary` instance from [@generic-arbitrary] serves as a basic example^[We modified class name to simplify.]

1. First we convert the type to its generic representation:

``` {.haskell}
genericArbitrary :: (Generic        a
                    ,Arbitrary (Rep a))
                 =>  Gen            a
genericArbitrary  = to <$> arbitrary
```

2. We take care of nullary constructors with:

``` {.haskell}
instance Arbitrary G.U1 where
  arbitrary = pure G.U1
```
3. For all fields arguments are recursively calling `Arbitrary` class method:

``` {.haskell}
instance Arbitrary c => Arbitrary (G.K1 i c) where
  gArbitrary = G.K1 <$> arbitrary
```
4. We skip metadata by the same recursive call:

``` {.haskell}
instance Arbitrary           f
      => Arbitrary (G.M1 i c f) where
  arbitrary = G.M1 <$> arbitrary
```

5. Given that all arguments of each constructor are joined by `:*:`,
we need to recursively delve there too:

``` {.haskell }
instance (Arbitrary  a,
         ,Arbitrary          b)
      =>  Arbitrary (a G.:*: b) where
  arbitrary = (G.:*:) <$> arbitrary <*> arbitrary
```

6. In order to sample all constructors with the same probability we compute
a number of constructor in each representation type with `SumLen` type family:

``` {.haskell}
type family SumLen a :: Nat where
  SumLen (a G.:+: b) = (SumLen a) + (SumLen b)
  SumLen a           = 1
```

Now that we have number of constructors computed, we can draw them with equal probability:
```{.haskell}
instance (Arbitrary        a
         ,Arbitrary                b
         ,KnownNat (SumLen a)
         ,KnownNat (SumLen         b)
         )
      =>  Arbitrary       (a G.:+: b) where
  arbitrary = frequency
    [ (lfreq, G.L1 <$> arbitrary)
    , (rfreq, G.R1 <$> arbitrary) ]
    where
      lfreq = fromIntegral
            $ natVal (Proxy :: Proxy (SumLen a))
      rfreq = fromIntegral
            $ natVal (Proxy :: Proxy (SumLen b))
```

Excellent piece of work, but non-terminating for recursive types
with average branching factor greater than 1 (and non-lazy tests, like checking `Eq` reflexivity.)

## Implementing with Generics

It is apparent from our previous considerations,
that we can reuse code from the existing generic
implementation when the budget is positive.
We just need to spend a dollar for
each constructor we encounter.

For the `Monoid` the implementation would be trivial,
since we can always use `mempty` and assume it is cheap:

```{.haskell #generic-less-arbitrary}
genericLessArbitraryMonoid :: (Generic               a
                              ,GLessArbitrary s (Rep a)
                              ,Monoid                a )
                           =>  CostGen        s      a
genericLessArbitraryMonoid  =
  pure mempty $$$? genericLessArbitrary
```

However we want to have fully generic implementation
that chooses the cheapest constructor even though the
datatype does not have monoid instance.

### Class for budget-conscious

When the budget is low,
we need to find the least costly constructor each time.

So to implement it as a type class `GLessArbitrary` that
is implemented for parts of the `Generic` `Rep`resentation type,
we will implement two methods:

1. `gLessArbitrary` is used for normal random data generation
2. `cheapest` is used when we run out of budget

```{.haskell #generic-less-arbitrary}
class GLessArbitrary s datatype where
  gLessArbitrary :: CostGen s (datatype p)
  cheapest       :: CostGen s (datatype p)

genericLessArbitrary :: (Generic               a
                        ,GLessArbitrary s (Rep a))
                     =>  CostGen        s      a
genericLessArbitrary = G.to <$> gLessArbitrary
```

### Helpful type family

First we need to compute minimum cost of the in each
branch of the type representation.
Instead of calling it _minimum cost_, we call this function `Cheapness`.

For this we need to implement minimum function at the type level:

```{.haskell #generic-instances}
type family Min m n where
  Min m n = ChooseSmaller (CmpNat m n) m n

type family ChooseSmaller (o::Ordering)
                          (m::Nat)
                          (n::Nat) where 
  ChooseSmaller 'LT m n = m
  ChooseSmaller 'EQ m n = m
  ChooseSmaller 'GT m n = n
```

so we can choose the cheapest^[We could add instances for :
```{.haskell #generic-instances}
type family Cheapness a :: Nat where
  Cheapness (a :*: b)  =
         Cheapness a + Cheapness b
  Cheapness (a :+: b)  =
    Min (Cheapness a) (Cheapness b)
  Cheapness  U1                      = 0
  <<flat-types>>
  Cheapness (K1 a other) = 1
  Cheapness (C1 a other) = 1
```

Since we are only interested in recursive types that can potentially blow out
our budget, we can also add cases for flat types since they seem the cheapest:

```{.haskell #flat-types}
Cheapness (S1 a (Rec0 Int       )) = 0
Cheapness (S1 a (Rec0 Scientific)) = 0
Cheapness (S1 a (Rec0 Double    )) = 0
Cheapness (S1 a (Rec0 Bool      )) = 0
Cheapness (S1 a (Rec0 Text.Text )) = 1
Cheapness (S1 a (Rec0 other     )) = 1
```

Of course we could also try to narrow generation of data structures by their size,
but that would complicate the code here, and also it would only work for regular
data structures that are not greatly affected by the passed state.

### Base case for each datatype

For each datatype, we first write a skeleton
code that first spends a coin,
and then checks whether we have enough
funds to go on expensive path,
or we are beyond our allocation
and need to generate from among the cheapest possible options.

```{.haskell #generic-less-arbitrary}
instance GLessArbitrary s       f
      => GLessArbitrary s (D1 m f) where 
  gLessArbitrary = do
    spend 1
    M1 <$> (cheapest $$$? gLessArbitrary)
  cheapest = M1 <$> cheapest
```

### Skipping over other metadata

First we safely ignore metadata by writing an instance:

```{.haskell #generic-instances}
instance GLessArbitrary s         f
      => GLessArbitrary s (G.C1 c f) where
  gLessArbitrary = G.M1 <$> gLessArbitrary
  cheapest       = G.M1 <$> cheapest

instance GLessArbitrary s         f
      => GLessArbitrary s (G.S1 c f) where
  gLessArbitrary = G.M1 <$> gLessArbitrary
  cheapest       = G.M1 <$> cheapest
```

### Counting constructors

In order to give equal draw chance for each constructor,
we need to count number of constructors
in each branch of sum type `:+:` so we can
generate each constructor with the same frequency:

```{.haskell #generic-less-arbitrary}
type family SumLen a :: Nat where
  SumLen (a G.:+: b) = SumLen a + SumLen b
  SumLen  a          = 1
```

### Base cases for `GLessArbitrary`

Now we are ready to define the instances of `GLessArbitrary` class.

We start with base cases `GLessArbitrary` for types with the same representation as unit type has only one result:

```{.haskell #generic-less-arbitrary}
instance GLessArbitrary s G.U1 where
  gLessArbitrary = pure G.U1
  cheapest       = pure G.U1
```

For the product of, we descend down the product of to reach each field,
and then assemble the result:

```{.haskell #generic-less-arbitrary}
instance (GLessArbitrary  s  a
         ,GLessArbitrary  s          b)
      =>  GLessArbitrary  s (a G.:*: b) where
  gLessArbitrary = (G.:*:) <$> gLessArbitrary
                           <*> gLessArbitrary
  cheapest       = (G.:*:) <$> cheapest
                           <*> cheapest
```

We recursively call instances of `LessArbitrary` for the types of fields:

```{.haskell #generic-less-arbitrary}
instance  LessArbitrary s         c
      => GLessArbitrary s (G.K1 i c) where
  gLessArbitrary = G.K1 <$> lessArbitrary
  cheapest       = G.K1 <$> lessArbitrary
```

### Selecting the constructor

We use code for selecting the constructor that
is taken after[@generic-arbitrary].

```{.haskell #generic-less-arbitrary}
instance (GLessArbitrary s    a
         ,GLessArbitrary s                  b
         ,KnownNat (SumLen    a)
         ,KnownNat (SumLen                  b)
         ,KnownNat (Cheapness a)
         ,KnownNat (Cheapness               b)
         )
      => GLessArbitrary  s   (a Generic.:+: b) where
  gLessArbitrary =
    frequency
      [ (lfreq, L1 <$> gLessArbitrary)
      , (rfreq, R1 <$> gLessArbitrary) ]
    where
      lfreq = fromIntegral
            $ natVal (Proxy :: Proxy (SumLen a))
      rfreq = fromIntegral
            $ natVal (Proxy :: Proxy (SumLen b))
  cheapest =
      if lcheap <= rcheap
         then L1 <$> cheapest
         else R1 <$> cheapest
    where
      lcheap, rcheap :: Int
      lcheap = fromIntegral
             $ natVal (Proxy :: Proxy (Cheapness a))
      rcheap = fromIntegral
             $ natVal (Proxy :: Proxy (Cheapness b))
```

# Conclusion

We show how to quickly define terminating test generators using generic programming.
This method may be transferred to other generic programming regimes
like Featherweight Go or Featherweight Java.

We recommend because it reduces time spent on making test generators
and improves user experience when a data structure
with no terminating constructors is defined.


# Bibliography

::::: {#refs}

:::::


# Appendix: Module headers {.unnumbered}

```{.haskell file=src/Test/LessArbitrary.hs}
{-# language DefaultSignatures          #-}
{-# language FlexibleInstances          #-}
{-# language FlexibleContexts           #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language InstanceSigs               #-}
{-# language Rank2Types                 #-}
{-# language PolyKinds                  #-}
{-# language MultiParamTypeClasses      #-}
{-# language MultiWayIf                 #-}
{-# language ScopedTypeVariables        #-}
{-# language TypeApplications           #-}
{-# language TypeOperators              #-}
{-# language TypeFamilies               #-}
{-# language TupleSections              #-}
{-# language UndecidableInstances       #-}
{-# language AllowAmbiguousTypes        #-}
{-# language DataKinds                  #-}
module Test.LessArbitrary(
    LessArbitrary(..)
  , oneof
  , choose
  , budgetChoose
  , CostGen(..)
  , (<$$$>)
  , ($$$?)
  , currentBudget
  , fasterArbitrary
  , genericLessArbitrary
  , genericLessArbitraryMonoid
  , flatLessArbitrary
  , spend
  , withCost
  , elements
  , forAll
  , sizedCost
  , StartingState(..)
  ) where

import qualified Data.HashMap.Strict        as Map
import qualified Data.Set                   as Set
import qualified Data.Vector                as Vector
import qualified Data.Text                  as Text
import           Control.Monad (replicateM)
import           Data.Scientific
import           Data.Proxy
import qualified Test.QuickCheck.Gen        as QC
import qualified Control.Monad.State.Strict as State
import           Control.Arrow (first, second)
import           Control.Monad.Trans.Class
import           System.Random (Random)
import           GHC.Generics    as G
import           GHC.Generics    as Generic
import           GHC.TypeLits
import           GHC.Stack
import qualified Test.QuickCheck as QC
import           Data.Hashable

import Test.LessArbitrary.Cost

<<starting-state>>
<<costgen>>

-- Mark a costly constructor with this instead of `<$>`
(<$$$>) :: (a -> b) -> CostGen s a -> CostGen s b
costlyConstructor <$$$> arg = do
  spend 1
  costlyConstructor <$> arg

<<spend>>

<<budget>>


withCost :: forall        s a.
            StartingState s
         => Int
         -> CostGen       s a
         -> QC.Gen          a
withCost cost gen = withCostAndState cost startingState gen

withCostAndState :: Int -> s -> CostGen s a -> QC.Gen a
withCostAndState cost state gen = runCostGen gen
  `State.evalStateT` (Cost cost, state)

<<generic-instances>>

<<generic-less-arbitrary>>

<<less-arbitrary-class>>

instance StartingState s
      => LessArbitrary s Bool where
  lessArbitrary = flatLessArbitrary

instance StartingState s
      => LessArbitrary s Int where
  lessArbitrary = flatLessArbitrary

instance StartingState s
      => LessArbitrary s Integer where
  lessArbitrary = flatLessArbitrary

instance StartingState s
      => LessArbitrary s Double where
  lessArbitrary = flatLessArbitrary

instance StartingState s
      => LessArbitrary s Char where
  lessArbitrary = flatLessArbitrary

instance (LessArbitrary s  k
         ,LessArbitrary s    v)
      =>  LessArbitrary s (k,v) where

instance (LessArbitrary s          k
         ,Ord                      k)
      =>  LessArbitrary s (Set.Set k) where
  lessArbitrary = Set.fromList <$> lessArbitrary

instance (LessArbitrary s              k
         ,Eq                           k
         ,Ord                          k
         ,Hashable                     k 
         ,LessArbitrary s                v)
      =>  LessArbitrary s (Map.HashMap k v) where
  lessArbitrary =  Map.fromList
               <$> lessArbitrary

instance StartingState s
      => LessArbitrary  s Scientific where
  lessArbitrary =
    scientific <$> lessArbitrary
               <*> lessArbitrary

<<arbitrary-implementation>>

flatLessArbitrary :: QC.Arbitrary a
                  => CostGen    s a
flatLessArbitrary  = CostGen $ lift QC.arbitrary

instance LessArbitrary s                a
      => LessArbitrary s (Vector.Vector a) where
  lessArbitrary = Vector.fromList <$> lessArbitrary

<<lifting-arbitrary>>

```

# Appendix: lifting classic `Arbitrary` functions {.unnumbered}

Below are functions and instances
that are lightly adjusted variants
of original implementations in `QuickCheck`[@quickcheck]

```{.haskell #lifting-arbitrary}
instance LessArbitrary s  a
      => LessArbitrary s [a] where
  lessArbitrary = pure [] $$$? do
    budget <- currentBudget
    len  <- choose (1,fromEnum budget)
    spend $ Cost len
    replicateM   len lessArbitrary

instance (QC.Testable            a
         ,LessArbitrary        s a)
      =>  QC.Testable (CostGen s a) where
  property = QC.property
           . sizedCost
```

Remaining functions are directly copied from `QuickCheck`[@quickcheck],
with only adjustment being their types and error messages:

```{.haskell #lifting-arbitrary}
forAll :: CostGen s a -> (a -> CostGen s b) -> CostGen s b
forAll gen prop = gen >>= prop

oneof   :: HasCallStack
        => [CostGen s a] -> CostGen s a
oneof [] = error
           "LessArbitrary.oneof used with empty list"
oneof gs = choose (0,length gs - 1) >>= (gs !!)

elements :: [a] -> CostGen s a
elements gs = (gs!!) <$> choose (0,length gs - 1)

choose      :: Random  a
            =>        (a, a)
            -> CostGen s a
choose (a,b) = CostGen $ lift $ QC.choose (a, b)

-- | Choose but only up to the budget (for array and list sizes)
budgetChoose :: CostGen s Int
budgetChoose  = do
  Cost b <- currentBudget
  CostGen $ lift $ QC.choose (1, b)

-- | Version of `suchThat` using budget instead of sized generators.
cg `suchThat` pred = do
  result <- cg
  if pred result
     then return result
     else do
       spend 1
       cg `suchThat` pred
```

This key function,
chooses one of the given generators, with a weighted random distribution.
The input list must be non-empty. Based on QuickCheck[@quickcheck].

```{.haskell .literate #lifting-arbitrary}
frequency   :: HasCallStack
            => [(Int, CostGen s a)]
            ->        CostGen s a
frequency [] =
  error $ "LessArbitrary.frequency "
       ++ "used with empty list"
frequency xs
  | any (< 0) (map fst xs) =
    error $ "LessArbitrary.frequency: "
         ++ "negative weight"
  | all (== 0) (map fst xs) =
    error $ "LessArbitrary.frequency: "
         ++ "all weights were zero"
frequency xs0 = choose (1, tot) >>= (`pick` xs0)
 where
  tot = sum (map fst xs0)

  pick n ((k,x):xs)
    | n <= k    = x
    | otherwise = pick (n-k) xs
  pick _ _  = error
    "LessArbitrary.pick used with empty list"
```

# Appendix: test suite {.unnumbered}

As observed in [@validity], it is important
to check basic properties of `Arbitrary` instance
to guarantee that shrinking terminates:

```{.haskell #arbitrary-laws}
shrinkCheck :: forall    term.
              (Arbitrary term
              ,Eq        term)
            =>           term
            -> Bool
shrinkCheck term =
  term `notElem` shrink term

arbitraryLaws :: forall    ty.
                (Arbitrary ty
                ,Show      ty
                ,Eq        ty)
              => Proxy     ty
              -> Laws
arbitraryLaws (Proxy :: Proxy ty) =
  Laws "arbitrary"
       [("does not shrink to itself",
         property (shrinkCheck :: ty -> Bool))]
```

For `LessArbitrary` we can also check that empty budget results in choosing a cheapest option,
but we need to provide a predicate that confirms what is actually the cheapest:

```{.haskell #less-arbitrary-check}
otherLaws :: [Laws]
otherLaws = [lessArbitraryLaws @() isLeaf]
  where
    isLeaf :: Tree Int -> Bool
    isLeaf (Leaf   _) = True
    isLeaf (Branch _) = False
```

```{.haskell #less-arbitrary-laws}
lessArbitraryLaws :: forall        s a.
                     LessArbitrary s a
                  =>                (a -> Bool)
                  -> Laws
lessArbitraryLaws cheapestPred =
    Laws "LessArbitrary"
         [("always selects cheapest",
           property $
             (prop_alwaysCheapest @s @a) cheapestPred)]

prop_alwaysCheapest :: forall s a.
                       LessArbitrary s a
                    =>                (a -> Bool)
                    -> Gen                  Bool
prop_alwaysCheapest cheapestPred =
  cheapestPred <$> (withCost @s @a) 0 lessArbitrary
```

Again some module headers:
```{.haskell file=src/Test/Arbitrary/Laws.hs}
{-# language DataKinds             #-}
{-# language FlexibleInstances     #-}
{-# language Rank2Types            #-}
{-# language MultiParamTypeClasses #-}
{-# language ScopedTypeVariables   #-}
{-# language TypeOperators         #-}
{-# language UndecidableInstances  #-}
{-# language AllowAmbiguousTypes   #-}
module Test.Arbitrary.Laws(
      arbitraryLaws
    ) where

import Data.Proxy
import Test.QuickCheck
import Test.QuickCheck.Classes
import qualified Data.HashMap.Strict as Map
import           Data.HashMap.Strict(HashMap)

<<arbitrary-laws>>
```

```{.haskell file=src/Test/LessArbitrary/Laws.hs}
{-# language DataKinds             #-}
{-# language FlexibleInstances     #-}
{-# language Rank2Types            #-}
{-# language MultiParamTypeClasses #-}
{-# language ScopedTypeVariables   #-}
{-# language TypeApplications      #-}
{-# language TypeOperators         #-}
{-# language UndecidableInstances  #-}
{-# language AllowAmbiguousTypes   #-}
module Test.LessArbitrary.Laws(
      lessArbitraryLaws
    ) where

import Data.Proxy
import Test.QuickCheck(Gen, property)
import Test.QuickCheck.Classes(Laws(..))
import Test.LessArbitrary
import qualified Data.HashMap.Strict as Map
import           Data.HashMap.Strict(HashMap)

<<less-arbitrary-laws>>
```

And we can compare the tests with `LessArbitrary` (which terminates fast, linear time):
```{.haskell file=test/less/LessArbitrary.hs}
<<test-file-header>>
<<test-less-arbitrary-version>>

<<test-file-laws>>
<<less-arbitrary-check>>
```

# Appendix: non-terminating test suite {.unnumbered}

Or with a generic `Arbitrary` (which naturally hangs):
```{.haskell file=test/nonterminating/NonterminatingArbitrary.hs}
<<test-file-header>>
<<tree-type-typical-arbitrary>>
otherLaws = []
<<test-file-laws>>
```

Here is the code:

```{.haskell #test-file-header}
{-# language FlexibleInstances     #-}
{-# language InstanceSigs          #-}
{-# language Rank2Types            #-}
{-# language MultiParamTypeClasses #-}
{-# language ScopedTypeVariables   #-}
{-# language TypeApplications      #-}
{-# language TypeOperators         #-}
{-# language UndecidableInstances  #-}
{-# language AllowAmbiguousTypes   #-}
{-# language DeriveGeneric         #-}
module Main where

import           Data.Proxy
import           Test.QuickCheck
import qualified Test.QuickCheck.Gen as QC
import qualified GHC.Generics as Generic
import           Test.QuickCheck.Classes

import           Test.LessArbitrary
import           Test.Arbitrary.Laws
import           Test.LessArbitrary.Laws

<<tree-type>>
```

```{.haskell #test-less-arbitrary-version}
instance (Arbitrary             a
         ,LessArbitrary s       a)
      =>  LessArbitrary s (Tree a) where
  lessArbitrary = genericLessArbitrary

instance (LessArbitrary () (Tree a)
         ,Arbitrary              a)
      => Arbitrary         (Tree a) where
  arbitrary = fasterArbitrary @() @(Tree a)
  shrink    = recursivelyShrink
```

```{.haskell #test-file-laws}

main :: IO ()
main = do
  lawsCheckMany
    [("Tree",
      [arbitraryLaws (Proxy :: Proxy (Tree Int))
      ,eqLaws        (Proxy :: Proxy (Tree Int))
      ] <> otherLaws)]
```

```{.haskell file=src/Test/LessArbitrary/Cost.hs}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Test.LessArbitrary.Cost where

<<cost>>
```

# Appendix: convenience functions provided with the module {.unnumbered}

Then we limit our choices when budget is tight:
```{.haskell #budget}
currentBudget :: CostGen s Cost
currentBudget = fst <$> CostGen State.get
```

```{.haskell #budget .hidden}
-- unused: loop breaker message type name
-- FIXME: use to make nicer error message
type family ShowType k where
  ShowType (D1 ('MetaData name _ _ _) _) = name
  ShowType  other                        = "unknown type"

showType :: forall                      a.
            (Generic                    a
            ,KnownSymbol (ShowType (Rep a)))
         => String
showType  = symbolVal (Proxy :: Proxy (ShowType (Rep a)))
```
